definition module debug

import StdEnv, circuitDefs

:: Debug = DEBUG (CompSpecifics Debug) ComponentID [Debug_Connect] [Debug_Connect] Placement

:: Debug_Connect = CONNECT WireID WireType

:: Debug_Wire = WIRE WireID Line WireType

CheckConnections :: !Circuit [WireID] -> (Bool, [WireID])

instance == Component
instance == CompSpecifics a  | == a
instance == Placement
instance == Connection
ConvertToCirc :: ![Debug] -> [Component]
ConvertToComp :: !Debug -> Component
ConvertSpec :: !(CompSpecifics Debug) -> (CompSpecifics Component)
ConvertToConnect :: !Debug_Connect -> Connection
ConvertToWires :: ![Debug_Wire] -> [Wire]
ConvertToWire :: !Debug_Wire -> Wire
instance == Debug
instance == Debug_Connect
instance == Wire
instance == WireType
instance == Type
