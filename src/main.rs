#![feature(unchecked_shifts, let_chains)]
#![allow(clippy::cast_sign_loss)]

macro_rules! color {
	($rgb:expr) => {
		Color32::from_rgb(
			((unsafe { u32::unchecked_shr($rgb, 16) }) & 0xff) as u8,
			((unsafe { u32::unchecked_shr($rgb, 8) }) & 0xff) as u8,
			($rgb & 0xff) as u8,
		)
	};
}

use clap::{Parser, Subcommand};
use eframe::{
	egui::{self, Align2, Color32, Context, FontId, Image, Rect, Rounding, Sense, Ui, Vec2},
	epaint::Hsva,
};

use chess::{
	board::{
		moves::{CheckState, Move, PlayedMove},
		pieces::Color,
		Board, Pos,
	},
	engine::Engine,
};

fn main()
{
	dotenv::dotenv().expect("Could not parse variables from .env file!");
	env_logger::init();

	let args = Arguments::parse();
	match args.command.unwrap_or(Command::Play)
	{
		Command::Play => start_gui_game().unwrap_or_else(|err| panic!("{err}")),
		Command::Perft { depth } =>
		{
			let mut board = Board::from_fen_string(&args.fen).expect("Invalid FEN string!");
			let perft_results = board.perft(depth);
			println!("{perft_results}");
		}
	}
}

fn start_gui_game() -> Result<(), eframe::Error>
{
	let options = eframe::NativeOptions {
		viewport: egui::ViewportBuilder::default().with_inner_size([1280.0, 720.0]),
		default_theme: eframe::Theme::Dark,
		follow_system_theme: false,
		..Default::default()
	};

	log::info!("starting");
	eframe::run_native(
		"Chess",
		options,
		Box::new(|cc| {
			egui_extras::install_image_loaders(&cc.egui_ctx);
			Box::<Application>::default()
		}),
	)
}

struct Application
{
	dark_square_color: Hsva,
	light_square_color: Hsva,

	board: Board,

	last_interacted_pos: Option<Pos>,
	dragging_index: Option<usize>,

	loadable_fen: String,
	current_fen: String,

	side_in_check: Option<Color>,

	engine: Engine,
	bot_plays_white: bool,
	bot_plays_black: bool,

	debug_mode: bool,
}
impl Default for Application
{
	fn default() -> Self
	{
		Self {
			dark_square_color: hsva_from_color32(color!(0xb58863)),
			light_square_color: hsva_from_color32(color!(0xf0d9b5)),
			board: Board::default(),
			dragging_index: None,
			last_interacted_pos: None,
			current_fen: String::new(),
			loadable_fen: String::from("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"),

			side_in_check: None,

			engine: Engine,
			bot_plays_white: false,
			bot_plays_black: true,

			debug_mode: false,
		}
	}
}
impl Application
{
	fn last_move(&self) -> Option<&PlayedMove>
	{
		self.board.last_move()
	}

	fn play_move(&mut self, mov: Move)
	{
		let played_move = self.board.make_move(mov);
		log::info!("{} plays {}", played_move.piece().color, played_move);
		self.current_fen = self.board.fen_string();
		self.side_in_check =
			(played_move.check_state() != CheckState::None).then(|| !played_move.piece().color);
	}

	fn bot_plays_for(&self, color: Color) -> bool
	{
		match color
		{
			Color::White => self.bot_plays_white,
			Color::Black => self.bot_plays_black,
		}
	}

	fn draw_left_panel(&mut self, ui: &mut Ui)
	{
		ui.vertical(|ui| {
			ui.set_width(300.0);
			let label = ui.label("FEN String: ");
			ui.text_edit_singleline(&mut self.loadable_fen)
				.labelled_by(label.id);

			let load_fen_button = ui.button("Load FEN");
			if load_fen_button.clicked()
			{
				self.board =
					Board::from_fen_string(&self.loadable_fen).expect("Invalid FEN string!");
				self.current_fen = self.board.fen_string();
			}

			ui.heading("Game information:");
			ui.label(format!("{} to move", self.board.to_move()));
			if !self.current_fen.is_empty()
			{
				ui.label(format!("Current Position:\n{}", self.current_fen));
			}
			if let Some(last_move) = self.last_move()
			{
				ui.label(format!("Last move: {last_move}"));
				let undo_button = ui.button("Undo");
				if undo_button.clicked()
				{
					self.board.unmake_move();
				}
			}

			if let Some(en_passant) = self.board.en_passant_target()
			{
				ui.label(format!("En passant target square: {en_passant}"));
			}

			ui.heading("Last interaction:");
			if let Some(last_pos) = self.last_interacted_pos
			{
				ui.label(format!(
					"{}\n({}, {})\n{}",
					last_pos,
					last_pos.file(),
					last_pos.rank(),
					last_pos.index()
				));
			}
		});
	}

	fn draw_right_panel(&mut self, ui: &mut Ui)
	{
		ui.vertical(|ui| {
			ui.horizontal(|ui| {
				let label = ui.label("Dark square color: ");
				ui.color_edit_button_hsva(&mut self.dark_square_color)
					.labelled_by(label.id);
			});
			ui.horizontal(|ui| {
				let label = ui.label("Light square color: ");
				ui.color_edit_button_hsva(&mut self.light_square_color)
					.labelled_by(label.id);
			});

			ui.checkbox(&mut self.bot_plays_white, "Bot makes white moves");
			ui.checkbox(&mut self.bot_plays_black, "Bot makes black moves");
			ui.checkbox(&mut self.debug_mode, "Debug mode");
		});
	}

	fn draw_board(&mut self, ui: &Ui, ctx: &Context, board_rect: Rect, tile_size: f32)
	{
		// draw board
		let painter = ui.painter();

		for i in 0..64
		{
			let is_held = self.dragging_index.is_some_and(|it| it == i);
			let board_pos = Pos::from_index(i);

			let tile_rect = Rect::from_min_size(
				board_rect.left_top()
					+ (Vec2::new(
						f32::from(board_pos.file()),
						f32::from(board_pos.top_down_rank()),
					) * tile_size),
				Vec2::splat(tile_size),
			);

			// base square
			painter.rect_filled(
				tile_rect,
				Rounding::ZERO,
				if (board_pos.file() + board_pos.rank()) % 2 == 0
				{
					self.dark_square_color
				}
				else
				{
					self.light_square_color
				},
			);

			// green highlight for currently moving piece shadow and last move source square
			if (is_held && self.board[i].is_some())
				|| self.last_move().is_some_and(|mv| mv.from().index() == i)
			{
				painter.rect_filled(
					tile_rect,
					Rounding::ZERO,
					Color32::from_rgba_premultiplied(20, 75, 20, 255 / 2),
				);
			}

			// darker green highlight for last move destination square
			if self.last_move().is_some_and(|mv| mv.to().index() == i)
			{
				painter.rect_filled(
					tile_rect,
					Rounding::ZERO,
					Color32::from_rgba_premultiplied(20, 50, 20, 255 / 2),
				);
			}

			// red highlight for current drag target square
			if self.dragging_index.is_some()
				&& ctx
					.pointer_latest_pos()
					.is_some_and(|pos| tile_rect.contains(pos))
			{
				painter.rect_filled(
					tile_rect,
					Rounding::ZERO,
					Color32::from_rgba_premultiplied(100, 15, 15, 255 / 2),
				);
			}

			if self.debug_mode
			{
				painter.text(
					tile_rect.left_top(),
					Align2::LEFT_TOP,
					board_pos.index().to_string(),
					FontId::monospace(10.0),
					Color32::BLACK,
				);
				painter.text(
					tile_rect.left_top() + Vec2::new(0.0, 15.0),
					Align2::LEFT_TOP,
					format!("({}, {})", board_pos.file(), board_pos.top_down_rank()),
					FontId::monospace(10.0),
					Color32::BLACK,
				);
			}

			let text_color = Color32::from(
				if (board_pos.file() + board_pos.rank()) % 2 == 0
				{
					self.light_square_color
				}
				else
				{
					self.dark_square_color
				},
			);

			// file letters on rank 1
			if board_pos.rank() == 0
			{
				painter.text(
					tile_rect.left_bottom() + Vec2::new(3.0, 0.0),
					Align2::LEFT_BOTTOM,
					board_pos.file_char().to_string(),
					FontId::proportional(15.0),
					text_color,
				);
			}

			// rank numbers on h file
			if board_pos.file() == 7
			{
				painter.text(
					tile_rect.right_top() + Vec2::new(-3.0, 1.0),
					Align2::RIGHT_TOP,
					(board_pos.rank() + 1).to_string(),
					FontId::proportional(15.0),
					text_color,
				);
			}

			// draw static pieces
			if let Some(piece) = self.board[i]
			{
				if piece.is_king() && self.side_in_check.is_some_and(|color| color == piece.color)
				{
					painter.rect_filled(
						tile_rect,
						Rounding::ZERO,
						Color32::from_rgba_premultiplied(100, 15, 15, 255 / 2),
					);
				}

				if is_held
				{
					Image::new(piece.icon())
						.tint(Color32::from_rgba_premultiplied(0x7f, 0x7f, 0x7f, 0x7f))
						.paint_at(ui, tile_rect);
				}
				else
				{
					Image::new(piece.icon()).paint_at(ui, tile_rect);
				}
			}

			// draw legal move target circles
			if self.board.legal_moves().iter().any(|mv| {
				self.dragging_index
					.is_some_and(|drag_idx| drag_idx == mv.from.index())
					&& mv.to == board_pos
			})
			{
				painter.circle_filled(
					tile_rect.center(),
					tile_size / 4.0,
					Color32::from_rgba_premultiplied(100, 20, 20, 0x7f),
				);
			}
		}

		// draw held piece
		if let Some(Some(piece)) = self.dragging_index.map(|i| self.board[i])
		{
			if let Some(cursor_pos) = ctx.pointer_latest_pos()
			{
				Image::new(piece.icon()).paint_at(
					ui,
					Rect::from_center_size(cursor_pos, Vec2::splat(tile_size)),
				);
			}
		}
	}
}
impl eframe::App for Application
{
	fn update(&mut self, ctx: &Context, _frame: &mut eframe::Frame)
	{
		egui::CentralPanel::default().show(ctx, |ui| {
			ui.horizontal(|ui| {
				self.draw_left_panel(ui);

				// reserve space for board
				let (board_id, board_rect) = ui.allocate_space(Vec2::splat(
					ctx.input(|i| i.viewport().inner_rect.unwrap().height() - 15.0),
				));

				self.draw_right_panel(ui);

				let tile_size = board_rect.height() / 8.0;
				self.draw_board(ui, ctx, board_rect, tile_size);

				// interaction
				if self.bot_plays_for(self.board.to_move())
				{
					let bot_move = self.engine.generate_move(&self.board);
					if self.board.legal_moves().contains(&bot_move)
					{
						self.play_move(bot_move);
					}
					else
					{
						log::warn!("Bot attempted to play illegal move: {}", bot_move);
					}
				}

				let response = ui.interact(board_rect, board_id, Sense::click_and_drag());
				if let Some(pointer_pos) = response.interact_pointer_pos()
				{
					let board_pos = Pos::from((pointer_pos - board_rect.left_top()) / tile_size);

					if response.drag_started()
						&& let Some(piece) = self.board[board_pos]
						&& self.board.to_move() == piece.color
						&& !self.bot_plays_for(self.board.to_move())
					{
						self.dragging_index = Some(board_pos.index());
						self.last_interacted_pos = Some(Pos::from_index(board_pos.index()));
					}
					if response.drag_stopped()
					{
						if let Some(old_index) = self.dragging_index
						{
							self.last_interacted_pos = Some(board_pos);

							let move_attempt = Move::new(Pos::from_index(old_index), board_pos);
							if self.board.legal_moves().contains(&move_attempt)
							{
								self.play_move(move_attempt);
							}
							self.dragging_index = None;
						}
					}
				}
			});
		});
	}
}

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Arguments
{
	#[command(subcommand)]
	command: Option<Command>,

	#[arg(
		short,
		long,
		default_value = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
	)]
	fen: String,
}

#[derive(Subcommand, Debug)]
#[command(about, long_about = None)]
enum Command
{
	Play,
	Perft
	{
		depth: u32,
	},
}
impl Default for Command
{
	fn default() -> Self
	{
		Self::Play
	}
}

fn hsva_from_color32(color: Color32) -> Hsva
{
	Hsva::from_srgb([color.r(), color.g(), color.b()])
}
