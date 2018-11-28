# Readme

## Project

Starting with Allsvenskan seasons 2017 & 2018. Validation results can then be compared to a model trained on data from other leagues as well.

* Are information from other leagues transferable to Allsvenskan?
* Or is it better traning on as much data as possible?

## Features

* Preceeding n passes
    + length
    + direction
    + zone_id
    + pos_x, pos_y
    + type, assists can be written as extra attacking, key or other (attacking or non attacking)
    + Opening
    + Succeeding an opening
    + Succeeding a dribble (player or goalkeeper)
    + Standart
* Possession time before shot
* Number of passes before shot
* Number of accurate passes before shot
* Speed
    + Attack
    + Vertical
    + Horizontal
* Distance
    + Attack
    + Vertical
    + Horizontal
* Start of attack position
    + Coordinates
    + Zone
* Shot succeeding a dribble
    + of player
    + of goalkeeper
* Succeeding an opening
* Body name
* Correct foot
* Second
* Shot angle & distance
* Type of attack, i.e. free kick, corner, open play, counter attack.
* Standart (i.e. type of kick)
* Shot after reflection in wood/keeper/player
* Shot after challenge
* Shot nbr in possession
* Number of shots per possession (to adjust xG)
* Game state
* Home team