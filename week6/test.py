from board import Direction, Rotation, Action
from exceptions import NoBlockException
from random import Random
import time


class Player:
    def choose_action(self, board):
        raise NotImplementedError

class MinimiseHoles(Player):
    def __init__(self, seed=None):
        self.go_left = True

    def print_board(self, board):
        print("--------")
        for y in range(24):
            s = ""
            for x in range(10):

                print(board[y][x], end="")
            print(s, y)

    def choose_action(self, board):
        # clone all the boards
        clones, moves = self.possible_moves(board)
        scores = []
        # need to do the max function manually

        best_score = [9999999, 39, False]
        # score the 40 boards
        for i in range(len(clones)):
            board_array = self.make_board_array(clones[i])
            scores.append(self.score_board(board_array)[0])

            if best_score[0] >= self.score_board(board_array)[0]:
                best_score[0] = self.score_board(board_array)[0]
                best_score[1] = i
                best_score[2] = self.score_board(board_array)[1]

        # print("")
        # print(scores)
        # print(best_score)
        # print(moves[best_score[1]])

        if best_score[2]:
            print("discard")
            return Action.Discard
        else:
            return moves[best_score[1]]

    def possible_moves(self, board):
        cloned_boards = []
        moves_made = []
        for i in range(10):
            for j in range(4):
                sandbox = board.clone()
                moves = []
                rotations = []
                if j == 1:
                    rotations.append(Rotation.Clockwise)
                elif j == 2:
                    rotations.append(Rotation.Clockwise)
                    rotations.append(Rotation.Clockwise)
                elif j == 3:
                    rotations.append(Rotation.Anticlockwise)
                if i < 5:
                    for k in range(5-i):
                        moves.append(Direction.Left)
                else:
                    for k in range(i-5):
                        moves.append(Direction.Right)

                moves.append(Direction.Drop)

                try:
                    for rotation in rotations:
                        sandbox.rotate(rotation)
                    for move in moves:
                        sandbox.move(move)
                except NoBlockException as e:
                    print(e)

                moves_made.append(rotations+moves)
                cloned_boards.append(sandbox)

        return cloned_boards, moves_made

    def make_board_array(self, board):
        board_array = [["." for _ in range(10)] for _ in range(24)]
        for y in range(24):
            for x in range(10):
                if (x, y) in board.cells:
                    board_array[y][x] = "#"
                else:
                    board_array[y][x] = "."
        return board_array

    def score_board(self, board_array):
        # self.print_board(board_array)
        score = 0
        for i in range(24):
            for j in range(10):
                has_hole = False
                height = 24-i
                # check if the place in the array is empty but has a block below
                if board_array[i][j] == ".":
                    # check left
                    if j != 0 and board_array[i][j-1] == "#":
                        score += height**2
                    # check right
                    if j != 9 and board_array[i][j+1] == "#":
                        score += height**2
                    # check up
                    if i != 0 and board_array[i-1][j] == "#":
                        score += height**5

        return score, has_hole

SelectedPlayer = MinimiseHoles
