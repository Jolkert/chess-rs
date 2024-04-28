use std::{borrow::BorrowMut, fmt::Display};

use eframe::egui::Vec2;
use lazy_regex::Regex;

use crate::pieces::{Color, Piece, PieceType, SlidingAxis, WhiteBlackPair};

// TODO: check
#[derive(Debug)]
pub struct Board
{
	pieces: [Option<Piece>; 64],
	to_move: Color,
	en_passant_target: Option<Pos>,
	castle_legality: CastleLegality,
}
impl Default for Board
{
	fn default() -> Self
	{
		Self {
			pieces: [None; 64],
			to_move: Color::White,
			en_passant_target: None,
			castle_legality: CastleLegality::default(),
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
impl std::ops::Index<Pos> for Board
{
	type Output = Option<Piece>;

	fn index(&self, index: Pos) -> &Self::Output
	{
		&self.pieces[index.index()]
	}
}
impl std::ops::IndexMut<Pos> for Board
{
	fn index_mut(&mut self, index: Pos) -> &mut Self::Output
	{
		&mut self.pieces[index.index()]
	}
}
impl Board
{
	pub fn has_king_moved(&self) -> WhiteBlackPair<bool>
	{
		self.castle_legality.has_king_moved
	}
	pub fn has_a_rook_moved(&self) -> WhiteBlackPair<bool>
	{
		self.castle_legality.has_a_rook_moved
	}
	pub fn has_h_rook_moved(&self) -> WhiteBlackPair<bool>
	{
		self.castle_legality.has_h_rook_moved
	}

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
			'w' => Some(Color::White),
			'b' => Some(Color::Black),
			_ => None,
		}?;

		let castle_options = captures[3].chars().collect::<Vec<_>>();
		let white_kingside = castle_options.contains(&'K');
		let white_queenside = castle_options.contains(&'Q');
		let black_kingside = castle_options.contains(&'k');
		let black_queenside = castle_options.contains(&'q');

		let en_passant_str = &captures[4];
		let en_passant_target = (en_passant_str != "-").then(|| {
			let file = en_passant_str
				.chars()
				.nth(0)
				.map(|it| (it as u8) - 97)
				.unwrap();
			let rank = en_passant_str.chars().nth(1).unwrap().to_digit(10).unwrap();

			Pos::from_rank_file((8 - rank) as u8, file)
		});

		Some(Self {
			pieces,
			to_move,
			castle_legality: CastleLegality {
				has_king_moved: WhiteBlackPair::new(
					!white_kingside && !white_queenside,
					!black_kingside && !black_queenside,
				),
				has_a_rook_moved: WhiteBlackPair::new(!white_queenside, !black_queenside),
				has_h_rook_moved: WhiteBlackPair::new(!white_kingside, !black_kingside),
			},
			en_passant_target,
		})
	}

	pub fn to_move(&self) -> Color
	{
		self.to_move
	}

	pub fn en_passant_target(&self) -> Option<Pos>
	{
		self.en_passant_target
	}

	pub fn legal_moves(&mut self) -> Vec<Move>
	{
		let pseudo_legal_moves = self.pseudo_legal_moves_for_color(self.to_move);
		pseudo_legal_moves
			.into_iter()
			.filter(|it| !self.move_endangers_king(*it))
			.collect()
	}

	pub fn pseudo_legal_moves_for_color(&self, color: Color) -> Vec<Move>
	{
		// do i hate this? -morgan 2024-04-27
		self.pieces
			.iter()
			.enumerate()
			.filter_map(|(i, square)| {
				square
					.and_then(|piece| (piece.color == color).then_some((Pos::from_index(i), piece)))
			})
			.flat_map(|(pos, piece)| self.pseudo_legal_moves_for_piece(pos, piece))
			.collect::<Vec<_>>()
	}

	pub fn pseudo_legal_moves_for_piece(&self, position: Pos, piece: Piece) -> Vec<Move>
	{
		match piece.piece_type
		{
			PieceType::Pawn =>
			{
				let forward = piece.forward_vector();
				Vec::from([
					position + forward,
					position + forward + Vec2i::RIGHT,
					position + forward + Vec2i::LEFT,
					(position.rank() == 1 || position.rank() == 6)
						.then(|| position + (forward * 2))
						.flatten(),
				])
			}
			PieceType::Knight => Vec::from([
				position + Vec2i::new(2, 1),
				position + Vec2i::new(2, -1),
				position + Vec2i::new(-2, 1),
				position + Vec2i::new(-2, -1),
				position + Vec2i::new(1, 2),
				position + Vec2i::new(1, -2),
				position + Vec2i::new(-1, 2),
				position + Vec2i::new(-1, -2),
			]),
			PieceType::King => Vec::from([
				position + Vec2i::LEFT,
				position + Vec2i::UP,
				position + Vec2i::RIGHT,
				position + Vec2i::DOWN,
				position + Vec2i::new(1, 1),
				position + Vec2i::new(1, -1),
				position + Vec2i::new(-1, 1),
				position + Vec2i::new(-1, -1),
				position + Vec2i::new(2, 0),
				position + Vec2i::new(-2, 0),
			]),
			PieceType::Queen => self.sliding_piece_targets(position, SlidingAxis::Both),
			PieceType::Bishop => self.sliding_piece_targets(position, SlidingAxis::Diagonal),
			PieceType::Rook => self.sliding_piece_targets(position, SlidingAxis::Orthogonal),
		}
		// is this a good idea? -morgan 2024-04-27
		.into_iter()
		.flatten()
		.map(|target_pos| Move::new(position, target_pos))
		.filter(|mov| self.can_piece_play(piece, *mov))
		.collect()
	}

	pub fn make_move(&mut self, mov: Move) -> PlayedMove
	{
		let piece = self[mov.from].expect("Tried to make a move from an empty square!");
		let previous_castle_legality = self.castle_legality;
		let previous_en_passant = self.en_passant_target;

		let is_en_passant =
			piece.is_pawn() && self[mov.to].is_none() && mov.from.file() != mov.to.file();
		let is_castle = piece.piece_type == PieceType::King && mov.offset().file.abs() > 1;

		let capture_square = is_en_passant
			.then(|| (mov.to - piece.forward_vector()).expect("En passant broke"))
			.unwrap_or(mov.to);

		let capture_data = self[capture_square].map(|piece| Capture {
			pos: capture_square,
			piece,
		});

		self[mov.to] = self[mov.from].take();
		if is_en_passant
		{
			self[(mov.to - piece.forward_vector()).expect("En passant broke")] = None;
		}

		let castle_state = is_castle.then(|| {
			let direction = mov.offset().normalized();
			let (rook_position, state) = if direction == Vec2i::LEFT
			{
				(mov.from.to_left_edge(), CastleState::Queenside)
			}
			else
			{
				(mov.from.to_right_edge(), CastleState::Kingside)
			};

			self[(mov.from + direction).unwrap()] = self[rook_position].take();

			state
		});

		if piece.piece_type == PieceType::King
		{
			self.castle_legality.has_king_moved[piece.color] = true;
		}
		if piece.piece_type == PieceType::Rook
		{
			if !self.castle_legality.has_a_rook_moved[piece.color] && mov.from.file() == 0
			{
				self.castle_legality.has_a_rook_moved[piece.color] = true;
			}

			if !self.castle_legality.has_h_rook_moved[piece.color] && mov.from.file() == 7
			{
				self.castle_legality.has_h_rook_moved[piece.color] = true;
			}
		}

		self.en_passant_target = (piece.is_pawn() && mov.from.rank().abs_diff(mov.to.rank()) > 1)
			.then(|| mov.to - piece.forward_vector())
			.flatten();
		self.to_move = !self.to_move;

		let check_state = if self.is_king_in_check(!piece.color)
		{
			if self.pseudo_legal_moves_for_color(Color::White).is_empty()
			{
				CheckState::Checkmate
			}
			else
			{
				CheckState::Check
			}
		}
		else
		{
			CheckState::None
		};

		PlayedMove {
			mov,
			piece,
			capture: capture_data,
			castle_state,
			check_state,
			previous_castle_legality,
			previous_en_passant,
		}
	}

	pub fn unmake_move(&mut self, mov: &PlayedMove)
	{
		let _ = self[mov.from()].insert(mov.piece);
		self[mov.to()] = None;
		if let Some(capture) = mov.capture
		{
			let _ = self[capture.pos].insert(capture.piece);
		}

		if let Some(castle_state) = mov.castle_state
		{
			let rook_start_pos = match castle_state
			{
				CastleState::Kingside => mov.to().to_right_edge(),
				CastleState::Queenside => mov.to().to_left_edge(),
			};

			let _ = self[rook_start_pos].insert(Piece::new(mov.piece.color, PieceType::Rook));
			let rook_pos_after_castle = (mov.from() + castle_state.direction())
				.expect("Attempted to undo invalid castle move!");
			self[rook_pos_after_castle] = None;
		}

		self.to_move = mov.piece.color;
		self.castle_legality = mov.previous_castle_legality;
		self.en_passant_target = mov.previous_en_passant;
	}

	fn sliding_piece_targets(&self, from: Pos, axis: SlidingAxis) -> Vec<Option<Pos>>
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
		let is_castle = piece.piece_type == PieceType::King && mov.offset().file.abs() > 1;
		let target_piece = self[mov.to];

		let move_allowed = can_move_to && target_piece.is_none() && !is_castle;
		let capture_allowed = can_capture
			&& !is_castle
			&& (target_piece.is_some_and(|p| p.color != piece.color)
				|| self.en_passant_target.is_some_and(|sq| sq == mov.to));

		// this castle detection is a nightmare -morgan 2024-04-26
		let castle_allowed = if piece.piece_type == PieceType::King
			&& !self.castle_legality.has_king_moved[piece.color]
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

			// now i just dont really like this all that much
			// TODO: clean this up -morgan 2024-04-26
			let has_rook_moved = match rook_position.file()
			{
				0 => self.castle_legality.has_a_rook_moved[piece.color],
				7 => self.castle_legality.has_h_rook_moved[piece.color],
				_ => true,
			};

			if let Some(rook) = self[rook_position]
				&& rook.piece_type == PieceType::Rook
				&& !has_rook_moved
			{
				let rook_ending_position = (mov.from + movement_direction).unwrap();
				let valid_rook_targets =
					self.sliding_piece_targets(rook_position, SlidingAxis::Orthogonal);

				valid_rook_targets.contains(&Some(rook_ending_position))
					&& self.can_piece_play(rook, Move::new(rook_position, rook_ending_position))
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

	fn move_endangers_king(&mut self, mov: Move) -> bool
	{
		let played_move = self.make_move(mov);
		let result = self.is_king_in_check(!self.to_move);
		self.unmake_move(&played_move);
		result
	}

	fn is_king_in_check(&mut self, color: Color) -> bool
	{
		self.pseudo_legal_moves_for_color(!color)
			.iter()
			.any(|mov| mov.to == self.king_postion(color))
	}

	fn king_postion(&self, color: Color) -> Pos
	{
		Pos::from_index(
			self.pieces
				.iter()
				.enumerate()
				.find(|(_, square)| {
					square.is_some_and(|piece| {
						piece.color == color && piece.piece_type == PieceType::King
					})
				})
				.unwrap_or_else(|| panic!("{color} king not found!"))
				.0,
		)
	}
}

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
	mov: Move,
	piece: Piece,
	capture: Option<Capture>,
	castle_state: Option<CastleState>,
	check_state: CheckState,
	previous_castle_legality: CastleLegality,
	previous_en_passant: Option<Pos>,
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
			Some(CastleState::Kingside) => write!(f, "O-O"),
			Some(CastleState::Queenside) => write!(f, "O-O-O"),
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
pub enum CastleState
{
	Kingside,
	Queenside,
}
impl CastleState
{
	pub fn direction(self) -> Vec2i
	{
		match self
		{
			Self::Kingside => Vec2i::RIGHT,
			Self::Queenside => Vec2i::LEFT,
		}
	}
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
struct CastleLegality
{
	pub has_king_moved: WhiteBlackPair<bool>,
	pub has_a_rook_moved: WhiteBlackPair<bool>,
	pub has_h_rook_moved: WhiteBlackPair<bool>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Pos
{
	tile_index: u8,
}
impl From<Vec2> for Pos
{
	fn from(value: Vec2) -> Self
	{
		Self {
			tile_index: 8 * (value.y.clamp(0.0, 8.0) as u8)
				+ (value.x.clamp(0.0, 8.0) as u8).clamp(0, 7),
		}
	}
}
impl Pos
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
		char::from_u32(u32::from(self.file()) + 97).expect("Invalid file character!")
	}

	pub fn move_by(self, offset: Vec2i) -> Option<Self>
	{
		let (signed_rank, signed_file) = (i32::from(self.top_down_rank()), i32::from(self.file()));
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
			i32::from(self.file()) - i32::from(other.file()),
			i32::from(self.rank()) - i32::from(other.rank()),
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
impl Display for Pos
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		write!(f, "{}{}", self.file_char(), self.rank() + 1)
	}
}
impl std::ops::Add<Vec2i> for Pos
{
	type Output = Option<Self>;

	fn add(self, rhs: Vec2i) -> Self::Output
	{
		self.move_by(rhs)
	}
}
impl std::ops::Add<Vec2i> for Option<Pos>
{
	type Output = Self;

	fn add(self, rhs: Vec2i) -> Self::Output
	{
		self.and_then(|lhs| lhs + rhs)
	}
}
impl std::ops::Sub<Vec2i> for Pos
{
	type Output = Option<Self>;

	fn sub(self, rhs: Vec2i) -> Self::Output
	{
		self.move_by(-rhs)
	}
}
impl std::ops::Sub<Vec2i> for Option<Pos>
{
	type Output = Self;

	fn sub(self, rhs: Vec2i) -> Self::Output
	{
		self.and_then(|lhs| lhs - rhs)
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
	pub const LEFT: Self = Self::new(-1, 0);
	pub const UP: Self = Self::new(0, 1);
	pub const RIGHT: Self = Self::new(1, 0);
	pub const DOWN: Self = Self::new(0, -1);

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
