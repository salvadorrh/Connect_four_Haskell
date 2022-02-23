# Connect_four_Haskell
Created a two player Connect Four game, programmed in the Programming Language Haskell.

Model-View-Controller is a common used design pattern to separate internal representation of the information to the information presented to the user. Haskell always uses this Model-View-Control pattern, this since in this Programming Language I/O funtions can call non I/O funtions, but non I/O functions can't call I/O functions.

Taking this in account, this Connect Four program will have two modules, the Board module and the MainGame module. In the Board module will not contain I/O functions, it will be the M in the MVC. 
The MainGame module will contain the UI representation of the information to the user, so it will call non I/O functions that are in the Board module.
