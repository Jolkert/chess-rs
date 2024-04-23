#![feature(unchecked_shifts)]

pub mod board;

macro_rules! color {
	($rgb:expr) => {
		Color32::from_rgb(
			((unsafe { u32::unchecked_shr($rgb, 16) }) & 0xff) as u8,
			((unsafe { u32::unchecked_shr($rgb, 8) }) & 0xff) as u8,
			($rgb & 0xff) as u8,
		)
	};
}

use eframe::{
	egui::{self, Color32, Rect, Rounding, Stroke, Vec2},
	epaint::Hsva,
};
use log::info;

use crate::board::{Board, Piece, PieceColor, PieceType};

fn main() -> Result<(), eframe::Error>
{
	dotenv::dotenv().expect("Could not parse variables fromn .env file!");
	env_logger::init();

	let options = eframe::NativeOptions {
		viewport: egui::ViewportBuilder::default().with_inner_size([1080.0, 720.0]),
		default_theme: eframe::Theme::Dark,
		follow_system_theme: false,
		..Default::default()
	};

	info!("starting");
	let mut app = Application::default();
	app.board[0] = Some(Piece::new(PieceColor::White, PieceType::Queen));

	eframe::run_native(
		"Chess",
		options,
		Box::new(|cc| {
			egui_extras::install_image_loaders(&cc.egui_ctx);
			Box::new(app)
		}),
	)
}

struct Application
{
	dark_square_color: Hsva,
	light_square_color: Hsva,
	board: Board,
}

impl Default for Application
{
	fn default() -> Self
	{
		Self {
			dark_square_color: hsva_from_color32(color!(0xb58863)),
			light_square_color: hsva_from_color32(color!(0xf0d9b5)),
			board: Default::default(),
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
					ui.heading("Left Panel");
					ui.label("We can put cool debug info here");
				});

				// reserve space for board
				let (_, board_rect) = ui.allocate_space(Vec2::splat(
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
					let file = i % 8;
					let rank = i / 8;

					let top_left =
						board_rect.left_top() + (Vec2::new(rank as f32, file as f32) * tile_size);

					painter.rect(
						Rect::from_min_size(top_left, Vec2::splat(tile_size)),
						Rounding::ZERO,
						if (file + rank) % 2 == 0
						{
							self.light_square_color
						}
						else
						{
							self.dark_square_color
						},
						Stroke::NONE,
					);
				}
			});
		});
	}
}
