use eframe::egui::{include_image, ImageSource};

use crate::board::pieces::{Color, Piece, PieceType};

pub const WHITE_BISHOP: ImageSource = include_image!("../textures/white_bishop.svg");
pub const WHITE_KING: ImageSource = include_image!("../textures/white_king.svg");
pub const WHITE_KNIGHT: ImageSource = include_image!("../textures/white_knight.svg");
pub const WHITE_PAWN: ImageSource = include_image!("../textures/white_pawn.svg");
pub const WHITE_QUEEN: ImageSource = include_image!("../textures/white_queen.svg");
pub const WHITE_ROOK: ImageSource = include_image!("../textures/white_rook.svg");
pub const BLACK_BISHOP: ImageSource = include_image!("../textures/black_bishop.svg");
pub const BLACK_KING: ImageSource = include_image!("../textures/black_king.svg");
pub const BLACK_KNIGHT: ImageSource = include_image!("../textures/black_knight.svg");
pub const BLACK_PAWN: ImageSource = include_image!("../textures/black_pawn.svg");
pub const BLACK_QUEEN: ImageSource = include_image!("../textures/black_queen.svg");
pub const BLACK_ROOK: ImageSource = include_image!("../textures/black_rook.svg");

impl Piece
{
	pub const fn icon(self) -> ImageSource<'static>
	{
		match (self.color, self.piece_type)
		{
			(Color::White, PieceType::Bishop) => WHITE_BISHOP,
			(Color::White, PieceType::King) => WHITE_KING,
			(Color::White, PieceType::Knight) => WHITE_KNIGHT,
			(Color::White, PieceType::Pawn) => WHITE_PAWN,
			(Color::White, PieceType::Queen) => WHITE_QUEEN,
			(Color::White, PieceType::Rook) => WHITE_ROOK,

			(Color::Black, PieceType::Bishop) => BLACK_BISHOP,
			(Color::Black, PieceType::King) => BLACK_KING,
			(Color::Black, PieceType::Knight) => BLACK_KNIGHT,
			(Color::Black, PieceType::Pawn) => BLACK_PAWN,
			(Color::Black, PieceType::Queen) => BLACK_QUEEN,
			(Color::Black, PieceType::Rook) => BLACK_ROOK,
		}
	}
}
