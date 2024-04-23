#[derive(Debug)]
pub struct Board
{
	pieces: [Option<Piece>; 64],
}
impl Default for Board
{
	fn default() -> Self
	{
		Self { pieces: [None; 64] }
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

impl Board
{
	pub fn set_piece_at(&mut self, index: usize, piece: Option<Piece>) -> &mut Self
	{
		self.pieces[index] = piece;
		self
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PieceType
{
	Pawn,
	Bihsop,
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
