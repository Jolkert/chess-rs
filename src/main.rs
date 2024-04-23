#![feature(unchecked_shifts, let_chains)]

pub mod board;
pub mod icons;

macro_rules! color {
	($rgb:expr) => {
		Color32::from_rgb(
			((unsafe { u32::unchecked_shr($rgb, 16) }) & 0xff) as u8,
			((unsafe { u32::unchecked_shr($rgb, 8) }) & 0xff) as u8,
			($rgb & 0xff) as u8,
		)
	};
}

use board::BoardPos;
use eframe::{
	egui::{self, Align2, Color32, FontId, Image, Rect, Rounding, Sense, Stroke, Vec2},
	epaint::Hsva,
};
use log::info;

use crate::board::Board;

fn main() -> Result<(), eframe::Error>
{
	dotenv::dotenv().expect("Could not parse variables fromn .env file!");
	env_logger::init();

	let options = eframe::NativeOptions {
		viewport: egui::ViewportBuilder::default().with_inner_size([1280.0, 720.0]),
		default_theme: eframe::Theme::Dark,
		follow_system_theme: false,
		..Default::default()
	};

	info!("starting");
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
	last_interacted_pos: Option<BoardPos>,
	board: Board,
	held_index: Option<usize>,
	fen_string: String,
}

impl Default for Application
{
	fn default() -> Self
	{
		Self {
			dark_square_color: hsva_from_color32(color!(0xb58863)),
			light_square_color: hsva_from_color32(color!(0xf0d9b5)),
			board: Default::default(),
			held_index: None,
			last_interacted_pos: None,
			fen_string: String::from("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq h3 0 1"),
		}
	}
}

fn hsva_from_color32(color: Color32) -> Hsva
{
	Hsva::from_srgb([color.r(), color.g(), color.b()])
}

impl eframe::App for Application
{
	fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame)
	{
		egui::CentralPanel::default().show(ctx, |ui| {
			ui.horizontal(|ui| {
				// left panel
				ui.vertical(|ui| {
					let label = ui.label("FEN String: ");
					ui.text_edit_singleline(&mut self.fen_string)
						.labelled_by(label.id);

					let button = ui.button("Load FEN");
					if button.clicked()
					{
						self.board =
							Board::from_fen_string(&self.fen_string).expect("Invalid FEN string!");
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

				// reserve space for board
				let (board_id, board_rect) = ui.allocate_space(Vec2::splat(
					ctx.input(|i| i.viewport().inner_rect.unwrap().height() - 15.0),
				));

				// right panel
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
				});

				// draw board
				let painter = ui.painter();
				let tile_size = board_rect.height() / 8.0;
				for i in 0..64
				{
					let is_held = self.held_index.is_some_and(|it| it == i);
					let board_pos = BoardPos::from_index(i);

					let tile_rect = Rect::from_min_size(
						board_rect.left_top()
							+ (Vec2::new(
								board_pos.file() as f32,
								board_pos.top_down_rank() as f32,
							) * tile_size),
						Vec2::splat(tile_size),
					);

					painter.rect(
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
						Stroke::NONE,
					);
					if is_held && self.board[i].is_some()
					{
						painter.rect(
							tile_rect,
							Rounding::ZERO,
							Color32::from_rgba_premultiplied(20, 75, 20, 255 / 2),
							Stroke::NONE,
						);
					}

					// painter.text(
					// 	tile_rect.left_top(),
					// 	Align2::LEFT_TOP,
					// 	i.to_string(),
					// 	FontId::monospace(10.0),
					// 	Color32::BLACK,
					// );

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

					if let Some(piece) = self.board[i]
					{
						if is_held
						{
							Image::new(piece.icon())
								.tint(Color32::from_rgba_premultiplied(
									255 / 2,
									255 / 2,
									255 / 2,
									255 / 2,
								))
								.paint_at(ui, tile_rect);
						}
						else
						{
							Image::new(piece.icon()).paint_at(ui, tile_rect);
						}
					}
				}

				// draw held piece
				if let Some(Some(piece)) = self.held_index.map(|i| self.board[i])
				{
					if let Some(cursor_pos) = ctx.pointer_latest_pos()
					{
						Image::new(piece.icon()).paint_at(
							ui,
							Rect::from_center_size(cursor_pos, Vec2::splat(tile_size)),
						)
					}
				}

				// interaction
				let response = ui.interact(board_rect, board_id, Sense::click_and_drag());
				if let Some(pointer_pos) = response.interact_pointer_pos()
				{
					let board_pos =
						BoardPos::from((pointer_pos - board_rect.left_top()) / tile_size);

					if response.drag_started()
					{
						self.held_index = Some(board_pos.index())
					}
					if response.drag_stopped()
					{
						if let Some(old_index) = self.held_index
						{
							let new_index = board_pos.index();
							self.last_interacted_pos = Some(BoardPos::from_index(new_index));
							self.board[new_index] = self.board[old_index].take();
							self.held_index = None;
						}
					}
				}
			});
		});
	}
}
