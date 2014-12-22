module JavaScript.Key where

import Data.Char

data SpecialKey =
	  KeySpace
	| KeyEnter
	| KeyTab
	| KeyEsc
	| KeyBackspace
	| KeyShift
	| KeyControl
	| KeyAlt
	| KeyCapsLock
	| KeyNumLock
	| KeyArrowLeft
	| KeyArrowUp
	| KeyArrowRight
	| KeyArrowDown
	| KeyIns
	| KeyDel
	| KeyHome
	| KeyEnd
	| KeyPgUp
	| KeyPgDown
	| KeyF1
	| KeyF2
	| KeyF3
	| KeyF4
	| KeyF5
	| KeyF6
	| KeyF7
	| KeyF8
	| KeyF9
	| KeyF10
	| KeyF11
	| KeyF12
	| KeyPadDel
	| KeyPadIns
	| KeyPadEnd
	| KeyPadDown
	| KeyPadPgDown
	| KeyPadLeft
	| KeyPadRight
	| KeyPadHome
	| KeyPadUp
	| KeyPadPgUp
	| KeyPadAdd
	| KeyPadSub
	| KeyPadMul
	| KeyPadDiv
	| KeyPadEnter
	| KeyPadDot
	| KeyPad0
	| KeyPad1
	| KeyPad2
	| KeyPad3
	| KeyPad4
	| KeyPad5
	| KeyPad6
	| KeyPad7
	| KeyPad8
	| KeyPad9

class KeyCode a where
	toKeyCode :: a -> Int

instance KeyCode Int where
	toKeyCode = id

instance KeyCode Char where
	toKeyCode ' ' = toKeyCode KeySpace
	toKeyCode '\n' = toKeyCode KeyEnter
	toKeyCode '\t' = toKeyCode KeyTab

	toKeyCode c | isAlphaNum c = ord c

instance KeyCode SpecialKey where
	toKeyCode KeySpace = 32
	toKeyCode KeyEnter = 13
	toKeyCode KeyTab = 9
	toKeyCode KeyEsc = 27
	toKeyCode KeyBackspace = 8
	toKeyCode KeyShift = 16
	toKeyCode KeyControl = 17
	toKeyCode KeyAlt = 18
	toKeyCode KeyCapsLock = 20
	toKeyCode KeyNumLock = 144
	toKeyCode KeyArrowLeft = 37
	toKeyCode KeyArrowUp = 38
	toKeyCode KeyArrowRight = 39
	toKeyCode KeyArrowDown = 40
	toKeyCode KeyIns = 45
	toKeyCode KeyDel = 46
	toKeyCode KeyHome = 36
	toKeyCode KeyEnd = 35
	toKeyCode KeyPgUp = 33
	toKeyCode KeyPgDown = 34
	toKeyCode KeyF1 = 112
	toKeyCode KeyF2 = 113
	toKeyCode KeyF3 = 114
	toKeyCode KeyF4 = 115
	toKeyCode KeyF5 = 116
	toKeyCode KeyF6 = 117
	toKeyCode KeyF7 = 118
	toKeyCode KeyF8 = 119
	toKeyCode KeyF9 = 120
	toKeyCode KeyF10 = 121
	toKeyCode KeyF11 = 122
	toKeyCode KeyF12 = 123
	toKeyCode KeyPadDel = 46
	toKeyCode KeyPadIns = 45
	toKeyCode KeyPadEnd = 35
	toKeyCode KeyPadDown = 40
	toKeyCode KeyPadPgDown = 34
	toKeyCode KeyPadLeft = 37
	toKeyCode KeyPadRight = 39
	toKeyCode KeyPadHome = 36
	toKeyCode KeyPadUp = 38
	toKeyCode KeyPadPgUp = 33
	toKeyCode KeyPadAdd = 107
	toKeyCode KeyPadSub = 109
	toKeyCode KeyPadMul = 106
	toKeyCode KeyPadDiv = 111
	toKeyCode KeyPadEnter = 13
	toKeyCode KeyPadDot = 46
	toKeyCode KeyPad0 = 48
	toKeyCode KeyPad1 = 49
	toKeyCode KeyPad2 = 50
	toKeyCode KeyPad3 = 51
	toKeyCode KeyPad4 = 52
	toKeyCode KeyPad5 = 53
	toKeyCode KeyPad6 = 54
	toKeyCode KeyPad7 = 55
	toKeyCode KeyPad8 = 56
	toKeyCode KeyPad9 = 57
