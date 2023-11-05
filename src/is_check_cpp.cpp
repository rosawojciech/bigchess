#include <Rcpp.h>
using namespace Rcpp;

//' @name is_check_cpp
//' @title Check if a king is in check
//'
//' @description This function checks if a king is in check. It does this by
//'   checking if any of the opponent's pieces can move to the square currently
//'   occupied by the king.
//'
//' @details The function takes into account the current state of the game and
//'   the color of the king in question. It then checks all possible moves of
//'   the opponent's pieces to see if any could capture the king.
//'
//' @param position A matrix representing the current state of the chess board.
//' @param p An integer indicating the color of the king to check (1 for white,
//'   -1 for black).
//'
//' @return A boolean value indicating whether the king is in check (TRUE) or
//'   not (FALSE).
//'
//' @export
//'
//' @examples
//' # Initialize a chess board
//' position <- position.start()
//' # Check if the white king is in check
//' is_check_cpp(position, 1)
// [[Rcpp::export]]
bool is_check_cpp(NumericMatrix position, int p) {
  // Locate the king
  int king = -1;
  for (int i = 0; i < position.size(); i++) {
    if (position[i] == -p*6) {
      king = i;
      break;
    }
  }

  // If king is still -1, it was not found
  if (king == -1) {
    // Handle error
    throw std::runtime_error("King not found");
  }

   // Check for attacking enemy pawns
   int pawn_positions[2][2] = {{-1, -1}, {1, -1}};
   pawn_positions[0][0] *= -p;
   pawn_positions[1][0] *= -p;

   for (int i = 0; i < 2; i++) {
     int x = king / 8 + pawn_positions[i][0];
     int y = king % 8 + pawn_positions[i][1];
     if (x >= 0 && x < 8 && y >= 0 && y < 8 && position(y, x) == p) {
       return true;
     }
   }

   // Check for attacking enemy knights
   int knight_positions[8][2] = {{-2, -1}, {-2, 1}, {2, -1}, {2, 1}, {-1, -2}, {-1, 2}, {1, -2}, {1, 2}};

   for (int i = 0; i < 8; i++) {
     int x = king / 8 + knight_positions[i][0];
     int y = king % 8 + knight_positions[i][1];
     if (x >= 0 && x < 8 && y >= 0 && y < 8 && position(y, x) == p*3) {
       return true;
     }
   }

   // Check each orthogonal direction
   int rook_directions[4][2] = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};

   for (int i = 0; i < 4; i++) {
     int x = king / 8 + rook_directions[i][0];
     int y = king % 8 + rook_directions[i][1];
     while (x >= 0 && x < 8 && y >= 0 && y < 8 && position(y, x) == 0) {
       x += rook_directions[i][0];
       y += rook_directions[i][1];
     }
     if (x >= 0 && x < 8 && y >= 0 && y < 8 && (position(y, x) == p*4 || position(y, x) == p*5)) {
       return true;
     }
   }

   // Check each diagonal direction
   int bishop_directions[4][2] = {{-1, -1}, {-1, 1}, {1, -1}, {1, 1}};

   for (int i = 0; i < 4; i++) {
     int x = king / 8 + bishop_directions[i][0];
     int y = king % 8 + bishop_directions[i][1];
     while (x >= 0 && x < 8 && y >= 0 && y < 8) {
       if (position(y, x) != 0) {
         if (position(y, x) == p*2 || position(y, x) == p*5) {
           return true;
         }
         break;
       }
       x += bishop_directions[i][0];
       y += bishop_directions[i][1];
     }
   }

   // If no attacks are found, return false
   return false;
}
