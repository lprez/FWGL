module FWGL.Key where

data MouseButton = MouseLeft | MouseMiddle | MouseRight deriving (Eq, Show)

data Key =
          KeyA
        | KeyB
        | KeyC
        | KeyD
        | KeyE
        | KeyF
        | KeyG
        | KeyH
        | KeyI
        | KeyJ
        | KeyK
        | KeyL
        | KeyM
        | KeyN
        | KeyO
        | KeyP
        | KeyQ
        | KeyR
        | KeyS
        | KeyT
        | KeyU
        | KeyV
        | KeyW
        | KeyX
        | KeyY
        | KeyZ
        | Key0
        | Key1
        | Key2
        | Key3
        | Key4
        | Key5
        | Key6
        | Key7
        | Key8
        | Key9
	| KeySpace
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
        deriving (Eq, Show)
