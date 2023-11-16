#include <Rcpp.h>
using namespace Rcpp;

//' @name is_check_cpp
//' @title Test if a king is in check
//'
//' @description This function tests if a king is in check.
//'
//' @details The function looks only at squares from which enemy pieces could
//'   attack the king based on the king's current position. If an attacking
//'   enemy piece is found in one of these squares, the function returns `TRUE`.
//'
//' @param position An 8 x 8 matrix representing the current position.
//' @param p An integer indicating the color of the enemy pieces (1 for white,
//'   -1 for black).
//'
//' @return A Boolean indicating if the king is in check.
//'
//' @export
//'
//' @examples
//' # Initialize a chess board
//' position <- position.start()
//' # Is the white king in check?
//' is_check_cpp(position, -1)
// [[Rcpp::export]]
bool is_check_cpp(NumericMatrix position, int p) {
  // Locate the king
  int king = -1;
  for (int i = 0; i < position.size(); i++) {
    if (position(i / 8, i % 8) == -p*6) {
      king = i;
      break;
    }
  }

  // If king is still -1, it was not found
  if (king == -1) {
    // Handle error
    throw std::runtime_error("In is_check_cpp: king not found.");
  }

  // Print the king's position and value
  //std::cout << "King is at position (" << king / 8 << ", " << king % 8 << ") with value " << position(king / 8, king % 8) << std::endl;

  // Check for attacking enemy pawns
  int pawn_positions[2][2] = {{p, -p}, {p, p}};

  for (int i = 0; i < 2; i++) {
    int x = king / 8 + pawn_positions[i][0];
    int y = king % 8 + pawn_positions[i][1];
    if (x >= 0 && x < 8 && y >= 0 && y < 8 && position(x, y) == p) {
      //std::cout << "King is in check by a pawn at position (" << x << ", " << y << ") with value " << position(x, y) << std::endl;
      return true;
    }
  }


   // Check for attacking enemy knights
   int knight_positions[8][2] = {{-2, -1}, {-2, 1}, {2, -1}, {2, 1}, {-1, -2}, {-1, 2}, {1, -2}, {1, 2}};

   for (int i = 0; i < 8; i++) {
     int x = king / 8 + knight_positions[i][0];
     int y = king % 8 + knight_positions[i][1];
     if (x >= 0 && x < 8 && y >= 0 && y < 8 && position(x, y) == p*3) {
       //std::cout << "King is in check by a knight at position (" << x << ", " << y << ") with value " << position(x, y) << std::endl;
       return true;
     }
   }

   // Check each orthogonal direction
   int rook_directions[4][2] = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};

   for (int i = 0; i < 4; i++) {
     int x = king / 8 + rook_directions[i][0];
     int y = king % 8 + rook_directions[i][1];
     while (x >= 0 && x < 8 && y >= 0 && y < 8 && position(x, y) == 0) {
       x += rook_directions[i][0];
       y += rook_directions[i][1];
     }
     if (x >= 0 && x < 8 && y >= 0 && y < 8 && (position(x, y) == p*4 || position(x, y) == p*5)) {
       //std::cout << "King is in check by a rook or queen at position (" << x << ", " << y << ") with value " << position(x, y) << std::endl;
       return true;
     }
   }

   // Check each diagonal direction
   int bishop_directions[4][2] = {{-1, -1}, {-1, 1}, {1, -1}, {1, 1}};

   for (int i = 0; i < 4; i++) {
     int x = king / 8 + bishop_directions[i][0];
     int y = king % 8 + bishop_directions[i][1];
     while (x >= 0 && x < 8 && y >= 0 && y < 8) {
       if (position(x, y) != 0) {
         if (position(x, y) == p*2 || position(x, y) == p*5) {
           //std::cout << "King is in check by a bishop or queen at position (" << x << ", " << y << ") with value " << position(x, y) << std::endl;
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
