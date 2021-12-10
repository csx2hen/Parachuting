# Diving
This is a game that a diving enthusiast need to avoid obstacles in the water and try to dive deeper. In this game, you will play the role of diver and use the keyboard to control movement of the character. At the beginning of the game, the player begins diving and has full field of vision. There are some floating or swiming obstacles which the player requires to keep away from during the game. As the player dives deeper, the field of vision will become smaller, which is more dangerous. The game will fail if the player crashes into them. The player's maximum depth will be recorded.

## Goals
- [ ] Use the keyboard to control role's movement
- [ ] Generate obstacles randomly
- [ ] Adjust the field of vision according to depth
- [ ] Display a scoreboard
- [ ] Contain various obstacles over time
- [ ] Adjust speed of the game


## Install
```
git clone git@github.com:csx2hen/diving.git
cd diving
stack build
stack exec diving-exe
```


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
