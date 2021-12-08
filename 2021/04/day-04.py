
from typing import List


def boardFromText(input: str): [x.split() for x in input.splitlines()]


def bingo(numbers: List[int], boards: List[List[List[int]]]):

    resultRows = [[[]]]

    for i, n in enumerate(numbers):
        for j, board in enumerate(boards):
            if n in board:
                print(resultRows[i][j])
                # resultRows[i][j] = resultRows[i].append(n)

    return resultRows

