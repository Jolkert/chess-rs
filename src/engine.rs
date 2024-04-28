use rand::seq::SliceRandom;

use crate::board::{Board, Move};

pub struct Engine;
impl Engine
{
	pub fn generate_move(&self, board: &mut Board) -> Move
	{
		*board.legal_moves().choose(&mut rand::thread_rng()).unwrap()
	}
}
