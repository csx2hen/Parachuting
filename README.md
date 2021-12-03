# Parachute
## Introduction
This is a game that a skydiving enthusiast need to avoid obstacles in the sky and try to land successfully. In this game, you will play the role of parachutist and use the keyboard to control movement of the character. At the beginning of the game, the player begins landing. There are some flying obstacles which the player requires to keep away from during the game. The game will fail if the player crashes into them. The player wins when the parachutist lands successfully.


## Goals
- [ ] Use the keyboard to control role's movement
- [ ] Generate obstacles randomly
- [ ] Display a scoreboard
- [ ] Contain various obstacles over time
- [ ] Adjust speed of the game


## Libraries
- brick
- random
- containers
- System.IO
- Graphics.Vty


## Architecture
- Controller: This component is used to deal with user input (keyboard input) transforming user’s input into character’s movement in the game.
- Core: This component contains the main logic of the game, including moving the character according to the signal parsed by the controller, generating different kinds of obstacles, detecting collision. The state of this component will be demonstrated by UI in real time.
- UI: The component is built in Brick, which is the interface for showing the game view, including the character, obstacles and borders.


## Challenges
- When an input event is coming, our game will interrupt and handle this event, so the game will not continue if there are plenty of events. -> Try to handle this process in multiple threads or discard duplicated events.
- Time conflict. Most of our team menbers have several deadlines or interviews recently, so it's hard to find a time to work together. -> Try our best to find a time slot to discuss the function of each part and assign tasks to each menber. Then every menber can do individually.


## Progress
- Finished setting the initial project and environment.
- Learning the Brick package, making some demo using that.
- The current progress meets our expectation. We will put more time and effort into the project in the following days. The project can be expected to finish in time.


## Team Members
- Shen Chen (shc005@ucsd.edu)
- Ke Wan (kewan@ucsd.edu)
- Zehui Jiao (zjiao@ucsd.edu)
- Yizhi Wei (yiw068@ucsd.edu)
