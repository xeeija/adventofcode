
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


print(bingo([7, 4, 9], [
    [
        [22, 13, 17, 11, 0],
        [8, 2, 23, 4, 24],
        [21, 9, 14, 16, 7],
        [6, 10, 3, 18, 5],
        [1, 12, 20, 15, 19]
    ],
    [
        [3, 15, 0, 2, 22],
        [9, 18, 13, 17, 5],
        [19, 8, 7, 25, 23],
        [20, 11, 10, 24, 4],
        [14, 21, 16, 12, 6]
    ]
]))
