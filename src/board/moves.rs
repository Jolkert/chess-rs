use std::{fmt::Display, iter, str::FromStr};

use super::{
	pieces::{Color, Piece, PieceType},
	ParsePosError, Pos, Vec2i,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Move
{
	pub from: Pos,
	pub to: Pos,
	pub promotion: Option<PromotionPiece>,
}
impl Move
{
	pub fn new(from: Pos, to: Pos) -> Self
	{
		Self {
			from,
			to,
			promotion: None,
		}
	}

	pub fn promote(from: Pos, to: Pos, promotion_piece: PromotionPiece) -> Self
	{
		Self {
			from,
			to,
			promotion: Some(promotion_piece),
		}
	}

	pub fn with_promotion_piece(self, promotion_piece: PromotionPiece) -> Self
	{
		Self {
			to: self.to,
			from: self.from,
			promotion: Some(promotion_piece),
		}
	}

	pub fn into_all_promotions(self) -> [Self; 4]
	{
		[
			self.with_promotion_piece(PromotionPiece::Rook),
			self.with_promotion_piece(PromotionPiece::Bishop),
			self.with_promotion_piece(PromotionPiece::Knight),
			self.with_promotion_piece(PromotionPiece::Queen),
		]
	}

	pub fn offset(self) -> Vec2i
	{
		self.to.offset_from(self.from)
	}
}
impl Display for Move
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		write!(f, "{}{}", self.from, self.to)
	}
}
impl FromStr for Move
{
	type Err = ParseMoveError;

	fn from_str(s: &str) -> Result<Self, Self::Err>
	{
		if s.len() < 4 || s.len() > 5
		{
			Err(ParseMoveError)
		}
		else
		{
			let from = Pos::from_str(&s[0..=1])?;
			let to = Pos::from_str(&s[2..=3])?;
			let promotion = s.chars().nth(4).map(PromotionPiece::try_from).transpose()?;

			Ok(Self {
				from,
				to,
				promotion,
			})
		}
	}
}

#[derive(Debug, Clone, Copy)]
pub struct ParseMoveError;
impl From<ParsePosError> for ParseMoveError
{
	fn from(_value: ParsePosError) -> Self
	{
		Self
	}
}
impl From<()> for ParseMoveError
{
	fn from(_value: ()) -> Self
	{
		Self
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PromotionPiece
{
	Queen,
	Rook,
	Bishop,
	Knight,
}
impl PromotionPiece
{
	pub const LIST: [Self; 4] = [Self::Queen, Self::Knight, Self::Rook, Self::Bishop];
}
impl TryFrom<PieceType> for PromotionPiece
{
	type Error = ();

	fn try_from(value: PieceType) -> Result<Self, Self::Error>
	{
		match value
		{
			PieceType::Queen => Ok(Self::Queen),
			PieceType::Rook => Ok(Self::Rook),
			PieceType::Bishop => Ok(Self::Bishop),
			PieceType::Knight => Ok(Self::Knight),
			_ => Err(()),
		}
	}
}
impl TryFrom<char> for PromotionPiece
{
	type Error = ();

	fn try_from(value: char) -> Result<Self, Self::Error>
	{
		match value.to_ascii_lowercase()
		{
			'q' => Ok(Self::Queen),
			'r' => Ok(Self::Rook),
			'b' => Ok(Self::Bishop),
			'n' => Ok(Self::Knight),
			_ => Err(()),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PlayedMove
{
	pub mov: Move,
	pub piece: Piece,
	pub capture: Option<Capture>,
	pub castle_state: Option<CastleSide>,
	pub check_state: CheckState,
	pub previous_castle_legality: CastleLegality,
	pub previous_en_passant: Option<Pos>,
}
impl Display for PlayedMove
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		match self.castle_state
		{
			None =>
			{
				// ew -morgan 2024-05-09
				let mut ret = write!(
					f,
					"{}{}{}",
					self.piece.piece_type,
					self.capture.is_some().then_some("x").unwrap_or_default(),
					self.mov.to
				);

				if let Some(promotion) = self.mov.promotion
				{
					ret = write!(f, "{}", PieceType::from(promotion));
				}
				ret
			}
			Some(CastleSide::Kingside) => write!(f, "O-O"),
			Some(CastleSide::Queenside) => write!(f, "O-O-O"),
		}
	}
}
impl PlayedMove
{
	pub fn from(&self) -> Pos
	{
		self.mov.from
	}
	pub fn to(&self) -> Pos
	{
		self.mov.to
	}

	pub fn is_en_passant(&self) -> bool
	{
		self.piece.is_pawn()
			&& self
				.previous_en_passant
				.is_some_and(|ep_square| self.to() == ep_square)
	}

	pub fn revealed_squares(&self) -> impl Iterator<Item = Pos>
	{
		// (
		// 	self.capture.map(|c| c.pos),
		// 	self.is_en_passant()
		// 		.then(|| self.to() - self.piece.forward_vector()),
		// )

		// im afraid of there being a reason i originally made this do the capture position first
		iter::once(self.from()).chain(
			iter::once(
				self.is_en_passant()
					.then(|| self.to() - self.piece.forward_vector()),
			)
			.flatten(),
		)
	}

	pub fn check_state(&self) -> CheckState
	{
		self.check_state
	}
	pub fn piece(&self) -> Piece
	{
		self.piece
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Capture
{
	pub pos: Pos,
	pub piece: Piece,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CheckState
{
	None,
	Check,
	Checkmate,
}
impl Display for CheckState
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		write!(
			f,
			"{}",
			match self
			{
				Self::None => "",
				Self::Check => "+",
				Self::Checkmate => "#",
			}
		)
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CastleSide
{
	Kingside,
	Queenside,
}
impl CastleSide
{
	pub fn direction(self) -> Vec2i
	{
		match self
		{
			Self::Kingside => Vec2i::EAST,
			Self::Queenside => Vec2i::WEST,
		}
	}
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
// these are all independently variable from one another
// and i dont see the need to make the type a separate enum
// -morgan 2024-04-28
#[allow(clippy::struct_excessive_bools)]
pub struct CastleLegality
{
	pub white_kingside: bool,
	pub white_queenside: bool,
	pub black_kingside: bool,
	pub black_queenside: bool,
}
impl CastleLegality
{
	pub fn is_allowed(self, color: Color, side: CastleSide) -> bool
	{
		match (color, side)
		{
			(Color::White, CastleSide::Kingside) => self.white_kingside,
			(Color::White, CastleSide::Queenside) => self.white_queenside,
			(Color::Black, CastleSide::Kingside) => self.black_kingside,
			(Color::Black, CastleSide::Queenside) => self.black_queenside,
		}
	}

	pub fn disallow(&mut self, color: Color, side: CastleSide)
	{
		*self.value_of(color, side) = false;
	}

	pub fn disallow_for_color(&mut self, color: Color)
	{
		*self.value_of(color, CastleSide::Kingside) = false;
		*self.value_of(color, CastleSide::Queenside) = false;
	}

	fn value_of(&mut self, color: Color, side: CastleSide) -> &mut bool
	{
		match (color, side)
		{
			(Color::White, CastleSide::Kingside) => &mut self.white_kingside,
			(Color::White, CastleSide::Queenside) => &mut self.white_queenside,
			(Color::Black, CastleSide::Kingside) => &mut self.black_kingside,
			(Color::Black, CastleSide::Queenside) => &mut self.black_queenside,
		}
	}
}
impl Display for CastleLegality
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		if !self.white_kingside
			&& !self.white_queenside
			&& !self.black_kingside
			&& !self.black_queenside
		{
			write!(f, "-")
		}
		else
		{
			write!(
				f,
				"{}{}{}{}",
				if self.white_kingside { "K" } else { "" },
				if self.white_queenside { "Q" } else { "" },
				if self.black_kingside { "k" } else { "" },
				if self.black_queenside { "q" } else { "" }
			)
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MovementDirection {}
