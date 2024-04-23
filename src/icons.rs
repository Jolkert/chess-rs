use eframe::egui::{include_image, ImageSource};

use crate::board::{Piece, PieceColor, PieceType};

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
			(PieceColor::White, PieceType::Bishop) => WHITE_BISHOP,
			(PieceColor::White, PieceType::King) => WHITE_KING,
			(PieceColor::White, PieceType::Knight) => WHITE_KNIGHT,
			(PieceColor::White, PieceType::Pawn) => WHITE_PAWN,
			(PieceColor::White, PieceType::Queen) => WHITE_QUEEN,
			(PieceColor::White, PieceType::Rook) => WHITE_ROOK,

			(PieceColor::Black, PieceType::Bishop) => BLACK_BISHOP,
			(PieceColor::Black, PieceType::King) => BLACK_KING,
			(PieceColor::Black, PieceType::Knight) => BLACK_KNIGHT,
			(PieceColor::Black, PieceType::Pawn) => BLACK_PAWN,
			(PieceColor::Black, PieceType::Queen) => BLACK_QUEEN,
			(PieceColor::Black, PieceType::Rook) => BLACK_ROOK,
		}
	}
}
