use std::{fmt::Display, iter};

use super::{
	pieces::{Color, Piece},
	Pos, Vec2i,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Move
{
	pub from: Pos,
	pub to: Pos,
}
impl Move
{
	pub fn new(from: Pos, to: Pos) -> Self
	{
		Self { from, to }
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
		write!(f, "{} --> {}", self.from, self.to)
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
			None => write!(
				f,
				"{}{}{}",
				self.piece.piece_type,
				self.capture.is_some().then_some("x").unwrap_or_default(),
				self.mov.to
			),
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

		iter::once(self.capture.map(|c| c.pos))
			.chain(iter::once(
				self.is_en_passant()
					.then(|| self.to() - self.piece.forward_vector()),
			))
			.flatten()
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MovementDirection {}
