use std::{borrow::BorrowMut, fmt::Display};

use eframe::egui::{os, Vec2};
use lazy_regex::Regex;

#[derive(Debug)]
pub struct Board
{
	// TODO:
	// 1. castling options
	// 2. en passant
	// 3. legal move checking
	pieces: [Option<Piece>; 64],
	to_move: PieceColor,
	en_passant_target: Option<BoardPos>,
}
impl Default for Board
{
	fn default() -> Self
	{
		Self {
			pieces: [None; 64],
			to_move: PieceColor::White,
			en_passant_target: None,
		}
	}
}
impl std::ops::Index<usize> for Board
{
	type Output = Option<Piece>;

	fn index(&self, index: usize) -> &Self::Output
	{
		&self.pieces[index]
	}
}
impl std::ops::IndexMut<usize> for Board
{
	fn index_mut(&mut self, index: usize) -> &mut Self::Output
	{
		&mut self.pieces[index]
	}
}
impl std::ops::Index<BoardPos> for Board
{
	type Output = Option<Piece>;

	fn index(&self, index: BoardPos) -> &Self::Output
	{
		&self.pieces[index.index()]
	}
}
impl std::ops::IndexMut<BoardPos> for Board
{
	fn index_mut(&mut self, index: BoardPos) -> &mut Self::Output
	{
		&mut self.pieces[index.index()]
	}
}

impl Board
{
	fn fen_regex() -> &'static Regex
	{
		lazy_regex::regex!(
			r"(?<pieces>(?:[pnrbkqPNRBKQ1-8]+/){7}[pnrbkqPNRBKQ1-8]+)\s+(?<to_move>[wb])\s+(?<castle>\-|[KQkq]+)\s(?<en_passant>\-|[a-h][36])\s(?<halfmove>\d+)\s(?<fullmove>\d+)"
		)
	}
	pub fn from_fen_string(fen: &str) -> Option<Self>
	{
		let captures = Self::fen_regex().captures(fen)?;

		let (mut cursor_file, mut cursor_rank) = (0, 0);
		let mut pieces: [Option<Piece>; 64] = [None; 64];
		for ch in captures[1].chars()
		{
			if ch == '/'
			{
				cursor_rank += 1;
				cursor_file = 0;
			}
			else if let Some(skip) = ch.to_digit(10)
			{
				cursor_file += skip;
			}
			else
			{
				pieces[(cursor_rank * 8 + cursor_file) as usize] = Piece::from_char(ch);
				cursor_file += 1;
			}
		}

		let to_move = match captures[2].chars().nth(0)?
		{
			'w' => Some(PieceColor::White),
			'b' => Some(PieceColor::Black),
			_ => None,
		}?;

		Some(Self {
			pieces,
			to_move,
			en_passant_target: None,
		})
	}

	pub fn to_move(&self) -> PieceColor
	{
		self.to_move
	}

	pub fn en_passant_target(&self) -> Option<BoardPos>
	{
		self.en_passant_target
	}

	pub fn legal_moves(&self) -> Vec<Move>
	{
		// wow i really hate this - morgan 2024-04-23
		let movable_pieces = self.pieces.iter().enumerate().filter_map(|it| {
			it.1.map_or(None, |piece| {
				if piece.color == self.to_move
				{
					Some((it.0, piece))
				}
				else
				{
					None
				}
			})
		});

		let mut legal_moves = Vec::new();
		for (index, piece) in movable_pieces
		{
			let current_pos = BoardPos::from_index(index);
			let valid_targets = match piece.piece_type
			{
				PieceType::Pawn =>
				{
					let forward = piece.forward_vector();
					Vec::from([
						current_pos + forward,
						current_pos + forward + Vec2i::RIGHT,
						current_pos + forward + Vec2i::LEFT,
						(current_pos.rank() == 1 || current_pos.rank() == 6)
							.then(|| current_pos + (forward * 2))
							.flatten(),
					])
				}
				PieceType::Knight => Vec::from([
					current_pos + Vec2i::new(2, 1),
					current_pos + Vec2i::new(2, -1),
					current_pos + Vec2i::new(-2, 1),
					current_pos + Vec2i::new(-2, -1),
					current_pos + Vec2i::new(1, 2),
					current_pos + Vec2i::new(1, -2),
					current_pos + Vec2i::new(-1, 2),
					current_pos + Vec2i::new(-1, -2),
				]),
				PieceType::King => Vec::from([
					current_pos + Vec2i::LEFT,
					current_pos + Vec2i::UP,
					current_pos + Vec2i::RIGHT,
					current_pos + Vec2i::DOWN,
					current_pos + Vec2i::new(1, 1),
					current_pos + Vec2i::new(1, -1),
					current_pos + Vec2i::new(-1, 1),
					current_pos + Vec2i::new(-1, -1),
					current_pos + Vec2i::new(2, 0),
					current_pos + Vec2i::new(-2, 0),
				]),
				PieceType::Queen => self.sliding_piece_targets(current_pos, SlidingAxis::Both),
				PieceType::Bishop => self.sliding_piece_targets(current_pos, SlidingAxis::Diagonal),
				PieceType::Rook => self.sliding_piece_targets(current_pos, SlidingAxis::Orthogonal),
			};

			for target_pos in valid_targets.into_iter().flatten()
			{
				if self.can_piece_play(piece, Move::new(current_pos, target_pos))
				{
					legal_moves.push(Move::new(current_pos, target_pos));
				}
			}
		}

		legal_moves
	}

	pub fn make_move(&mut self, mov: Move)
	{
		let piece = self[mov.from].expect("Tried to make a move from an empty square!");
		let is_en_passant =
			piece.is_pawn() && self[mov.to].is_none() && mov.from.file() != mov.to.file();
		let is_castle = piece.piece_type == PieceType::King && mov.offset().file.abs() > 1;

		self[mov.to] = self[mov.from].take();
		if is_en_passant
		{
			self[(mov.to - piece.forward_vector()).expect("En passant broke")] = None;
		}

		if is_castle
		{
			let direction = mov.offset().normalized();
			let rook_position = if direction == Vec2i::LEFT
			{
				mov.from.to_left_edge()
			}
			else
			{
				mov.from.to_right_edge()
			};

			// log::info!("Move: {mov}; Direction: {direction}; Rook at: {rook_position}");

			self[(mov.from + direction).unwrap()] = self[rook_position].take();
		}

		self.en_passant_target = (piece.is_pawn() && mov.from.rank().abs_diff(mov.to.rank()) > 1)
			.then(|| mov.to - piece.forward_vector())
			.flatten();
		self.to_move = !self.to_move;

		log::info!("Move offset: {}", mov.offset());
	}

	fn sliding_piece_targets(&self, from: BoardPos, axis: SlidingAxis) -> Vec<Option<BoardPos>>
	{
		let mut targets = Vec::new();
		let mut cursor = from;

		let mut valid_offsets = Vec::new();
		if axis.orthogonal_allowed()
		{
			valid_offsets.append(
				Vec::from([Vec2i::LEFT, Vec2i::UP, Vec2i::RIGHT, Vec2i::DOWN]).borrow_mut(),
			);
		}
		if axis.diagonal_allowed()
		{
			valid_offsets.append(
				Vec::from([
					Vec2i::new(1, 1),
					Vec2i::new(1, -1),
					Vec2i::new(-1, 1),
					Vec2i::new(-1, -1),
				])
				.borrow_mut(),
			);
		}

		for offset in valid_offsets
		{
			while let Some(new_pos) = cursor + offset
			{
				targets.push(Some(new_pos));
				if self[new_pos].is_some()
				{
					break;
				}
				cursor = new_pos;
			}

			cursor = from;
		}
		targets
	}

	fn can_piece_play(&self, piece: Piece, mov: Move) -> bool
	{
		let can_move_to = !piece.is_pawn() || mov.from.file() == mov.to.file();
		let can_capture = !piece.is_pawn() || mov.from.file() != mov.to.file();
		let target_piece = self[mov.to];

		let move_allowed = can_move_to && target_piece.is_none();
		let capture_allowed = can_capture
			&& (target_piece.is_some_and(|p| p.color != piece.color)
				|| self.en_passant_target.is_some_and(|sq| sq == mov.to));

		// this castle detection is a nightmare -morgan 2024-04-26
		let castle_allowed = if piece.piece_type == PieceType::King
		{
			let movement_direction = mov.offset().normalized();
			let rook_position = if movement_direction == Vec2i::LEFT
			{
				mov.from.to_left_edge()
			}
			else
			{
				mov.from.to_right_edge()
			};

			// log::info!("{mov}; relevant rook at: {}", rook_position);

			if let Some(piece) = self[rook_position]
				&& piece.piece_type == PieceType::Rook
			{
				self.can_piece_play(
					piece,
					Move::new(rook_position, (mov.from + movement_direction).unwrap()),
				)
			}
			else
			{
				false
			}
		}
		else
		{
			false
		};

		move_allowed || capture_allowed || castle_allowed
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Vec2i
{
	pub file: i32,
	pub rank: i32,
}
impl Vec2i
{
	pub const LEFT: Self = Vec2i::new(-1, 0);
	pub const UP: Self = Vec2i::new(0, 1);
	pub const RIGHT: Self = Vec2i::new(1, 0);
	pub const DOWN: Self = Vec2i::new(0, -1);

	pub const fn new(file: i32, rank: i32) -> Self
	{
		Self { file, rank }
	}

	pub fn normalized(self) -> Self
	{
		Self::new(
			self.file.checked_div(self.file.abs()).unwrap_or_default(),
			self.rank.checked_div(self.rank.abs()).unwrap_or_default(),
		)
	}
}
impl std::ops::Neg for Vec2i
{
	type Output = Self;

	fn neg(self) -> Self::Output
	{
		Self::new(-self.file, -self.rank)
	}
}
impl std::ops::Mul<i32> for Vec2i
{
	type Output = Self;

	fn mul(self, rhs: i32) -> Self::Output
	{
		Self::new(self.file * rhs, self.rank * rhs)
	}
}
impl Display for Vec2i
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		write!(f, "({}, {})", self.file, self.rank)
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SlidingAxis
{
	Orthogonal,
	Diagonal,
	Both,
}
impl SlidingAxis
{
	pub fn orthogonal_allowed(self) -> bool
	{
		self == Self::Orthogonal || self == Self::Both
	}
	pub fn diagonal_allowed(self) -> bool
	{
		self == Self::Diagonal || self == Self::Both
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Move
{
	pub from: BoardPos,
	pub to: BoardPos,
}
impl Move
{
	pub fn new(from: BoardPos, to: BoardPos) -> Self
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BoardPos
{
	tile_index: u8,
}
impl From<Vec2> for BoardPos
{
	fn from(value: Vec2) -> Self
	{
		Self {
			tile_index: 8 * (value.y.clamp(0.0, 8.0) as u8)
				+ (value.x.clamp(0.0, 8.0) as u8).clamp(0, 7),
		}
	}
}
impl BoardPos
{
	pub const fn from_index(tile_index: usize) -> Self
	{
		Self {
			tile_index: tile_index as u8,
		}
	}
	pub const fn from_rank_file(rank: u8, file: u8) -> Self
	{
		Self {
			tile_index: 8 * rank + file,
		}
	}

	pub const fn rank(self) -> u8
	{
		7 - (self.tile_index / 8)
	}
	pub const fn top_down_rank(self) -> u8
	{
		self.tile_index / 8
	}
	pub const fn file(self) -> u8
	{
		self.tile_index % 8
	}
	pub const fn rank_file(self) -> (u8, u8)
	{
		(self.rank(), self.file())
	}
	pub const fn index(self) -> usize
	{
		self.tile_index as usize
	}

	pub fn file_char(self) -> char
	{
		char::from_u32(self.file() as u32 + 97).expect("Invalid file character!")
	}

	pub fn move_by(self, offset: Vec2i) -> Option<BoardPos>
	{
		let (signed_rank, signed_file) = (self.top_down_rank() as i32, self.file() as i32);
		let unclamped_pos = ((signed_rank - offset.rank), (signed_file + offset.file));
		let new_pos = ((0..=7).contains(&unclamped_pos.0) && (0..=7).contains(&unclamped_pos.1))
			.then(|| {
				Self::from_rank_file(
					unclamped_pos.0.clamp(0, 7) as u8,
					unclamped_pos.1.clamp(0, 7) as u8,
				)
			});

		new_pos.and_then(|pos| (pos != self).then_some(pos))
	}

	pub fn offset_from(self, other: Self) -> Vec2i
	{
		Vec2i::new(
			self.file() as i32 - other.file() as i32,
			self.rank() as i32 - other.rank() as i32,
		)
	}

	pub fn to_left_edge(self) -> Self
	{
		Self::from_rank_file(self.top_down_rank(), 0)
	}
	pub fn to_right_edge(self) -> Self
	{
		Self::from_rank_file(self.top_down_rank(), 7)
	}
}
impl Display for BoardPos
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		write!(f, "{}{}", self.file_char(), self.rank() + 1)
	}
}
impl std::ops::Add<Vec2i> for BoardPos
{
	type Output = Option<Self>;

	fn add(self, rhs: Vec2i) -> Self::Output
	{
		self.move_by(rhs)
	}
}
impl std::ops::Add<Vec2i> for Option<BoardPos>
{
	type Output = Self;

	fn add(self, rhs: Vec2i) -> Self::Output
	{
		self.and_then(|lhs| lhs + rhs)
	}
}
impl std::ops::Sub<Vec2i> for BoardPos
{
	type Output = Option<Self>;

	fn sub(self, rhs: Vec2i) -> Self::Output
	{
		self.move_by(-rhs)
	}
}
impl std::ops::Sub<Vec2i> for Option<BoardPos>
{
	type Output = Self;

	fn sub(self, rhs: Vec2i) -> Self::Output
	{
		self.and_then(|lhs| lhs - rhs)
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Piece
{
	pub piece_type: PieceType,
	pub color: PieceColor,
}
impl Piece
{
	pub fn new(color: PieceColor, piece_type: PieceType) -> Self
	{
		Self { color, piece_type }
	}

	pub fn from_char(fen_char: char) -> Option<Self>
	{
		let piece_type = match fen_char.to_ascii_lowercase()
		{
			'p' => PieceType::Pawn,
			'n' => PieceType::Knight,
			'b' => PieceType::Bishop,
			'r' => PieceType::Rook,
			'q' => PieceType::Queen,
			'k' => PieceType::King,
			_ => return None,
		};
		let color = if fen_char.is_ascii_uppercase()
		{
			PieceColor::White
		}
		else
		{
			PieceColor::Black
		};

		Some(Self::new(color, piece_type))
	}

	pub fn is_pawn(self) -> bool
	{
		self.piece_type == PieceType::Pawn
	}

	pub fn forward_vector(self) -> Vec2i
	{
		if self.color == PieceColor::White
		{
			Vec2i::new(0, 1)
		}
		else
		{
			Vec2i::new(0, -1)
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PieceType
{
	Pawn,
	Bishop,
	Rook,
	Knight,
	Queen,
	King,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PieceColor
{
	White,
	Black,
}
impl Display for PieceColor
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		write!(
			f,
			"{}",
			match self
			{
				Self::White => "White",
				Self::Black => "Black",
			}
		)
	}
}
impl std::ops::Not for PieceColor
{
	type Output = Self;

	fn not(self) -> Self::Output
	{
		match self
		{
			Self::White => Self::Black,
			Self::Black => Self::White,
		}
	}
}
