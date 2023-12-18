# CSE230 Project: Snake

> **Group Member**
>
> - [Jianke Yang](https://github.com/jiankeyang)
> - [Zhongming Yu](https://github.com/fishmingyu)
> - [Yuhang Xie](https://github.com/scarletxyh)
> - [Yulei Liu](https://github.com/lyl1551)

## Overview

This project aims to develop an interactive version of the classic arcade game Snake using Haskell, with a primary focus on utilizing the `brick` library for terminal-based UI. The game involves a snake moving around a plane, consuming food, and growing in length while avoiding collisions with itself or the game boundaries.

## Core Functions

- **Basic Gameplay Mechanics**: Implement the fundamental mechanics of Snake, including snake movement, food generation, and score tracking.
- **Terminal-Based UI**: Utilize the `brick` library to create a responsive and visually appealing terminal interface.
- **Game State Management**: Efficiently handle game state changes (e.g., snake growth, food consumption) in Haskell, showcasing functional programming paradigms.

## MileStone

<p align="center">
<img src="https://raw.githubusercontent.com/jiankeyang/CSE230-Project/main/snake.png" alt="drawing" width="60%"/>
</p>

### How to Run

```
stack build
stack run
stack test
```

### Final Features

- **Cover**: The game welcomes players with an eye-catching cover screen, featuring a pixel-art style title and a prompt to start the game. This screen sets the tone and immerses players right from the beginning.
- **Puzzle**: Challenging puzzles in the form of various barrier layouts are a core part of the game's experience. These include square barriers, cross barriers, diagonal barriers, and more, offering diverse challenges.
- **Random Map**: Each game session offers a new experience with randomly generated maps. The layout of barriers and obstacles changes, requiring players to adapt their strategy each time they play.
- **Dynamic Game Speed**:The game's speed dynamically adjusts as the player progresses, increasing the difficulty and excitement. This feature keeps players engaged and challenges their reflexive responses.
- **Timer**: A timer is displayed, adding a sense of urgency and challenge. It encourages players to beat their best times, adding a competitive edge to the game.
- **UnitTest**:The game's robustness is ensured through comprehensive unit testing, covering various aspects of the gameplay and mechanics. This guarantees a smooth and bug-free gaming experience.
- **Tips&Help**: A dedicated section provides players with helpful tips and guidance. Whether you're a beginner or looking to improve your strategy, this section offers valuable insights to enhance your gameplay.

### Collaboration

- Zhongming Yu: Random Map & Barrier Logic & Cover
- Jianke Yang: Dynamic game speed & Presentation
- Yuhang Xie:
- Yulei Liu: Timer & Tick Logic & Presentation

## Demo

<p align="center">
<img src="https://raw.githubusercontent.com/jiankeyang/CSE230-Project/main/snake.gif" alt="gif" width="60%"/>
</p>

## Source Acknowledgement

We have referred to [an existing snake project](https://github.com/samtay/snake/tree/master) for base game implementation. The above `Final Features` section lists our feature extensions based on the source framework.
