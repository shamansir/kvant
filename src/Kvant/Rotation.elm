module Kvant.Rotation exposing (..)


import Kvant.Symmetry exposing (..)
import Kvant.Neighbours exposing (..)
import Kvant.Direction as D exposing (Direction)


 -- 0 a.k.a. Original is facing West, then rotations go clockwise
 -- 90deg a.k.a. Quarter is facing North
 -- 180deg a.k.a. Half is facing East
 -- 270deg a.k.a. ThreeQuarters is facing South
 -- 360deg a.k.a. Original is facing West again
type Rotation
    = Original -- 0 or 360 degrees, Eest
    | Quarter -- 90 degrees, North
    | Half -- 180 degrees, South
    | ThreeQuarters -- 270 degrees, West
    | AnyHorizontal -- both Quarter (North) and ThreeQuarters (South) can suffice
    | AnyVertical -- both Original (East) and Half (West) can suffice
    | AnyQuarter -- any of: Original, Quarter, Half, ThreeQuarters
    -- TODO: Flipping ?


type alias RotationId = Int


rotate : Rotation -> Direction -> Direction
rotate r dir =
    case ( toId r, dir ) of
        ( 0, _  )   -> dir
        ( _, D.X  ) -> D.X

        ( 1, D.NW ) -> D.NE
        ( 1, D.N  ) -> D.E
        ( 1, D.NE ) -> D.SE
        ( 1, D.E  ) -> D.S
        ( 1, D.SE ) -> D.SW
        ( 1, D.S  ) -> D.W
        ( 1, D.SW ) -> D.NW
        ( 1, D.W  ) -> D.N

        ( 2, D.NW ) -> D.SE
        ( 2, D.N  ) -> D.S
        ( 2, D.NE ) -> D.SW
        ( 2, D.E  ) -> D.W
        ( 2, D.SE ) -> D.NW
        ( 2, D.S  ) -> D.N
        ( 2, D.SW ) -> D.SW
        ( 2, D.W  ) -> D.E

        ( 3, D.NW ) -> D.SW
        ( 3, D.N  ) -> D.W
        ( 3, D.NE ) -> D.NW
        ( 3, D.E  ) -> D.N
        ( 3, D.SE ) -> D.NE
        ( 3, D.S  ) -> D.E
        ( 3, D.SW ) -> D.SE
        ( 3, D.W  ) -> D.S

        ( _, _    ) -> dir


uniqueFor : Symmetry -> List Rotation
uniqueFor symmetry =
    case symmetry of
        I -> [ AnyHorizontal, AnyVertical ]
        T -> [ AnyHorizontal, Quarter, ThreeQuarters ]
        X -> [ AnyQuarter ]
        L -> [ Original, Quarter, Half, ThreeQuarters ]
        Diagonal -> [ AnyHorizontal, AnyVertical ]


apply : Symmetry -> Rotation -> Rotation
apply symmetry curRotation =
    case ( symmetry, curRotation ) of

        ( X, _ ) -> AnyQuarter

        ( T, Original ) -> AnyHorizontal
        ( T, Half ) -> AnyHorizontal
        ( T, _ ) -> curRotation

        ( I, Original ) -> AnyHorizontal
        ( I, Quarter ) -> AnyVertical
        ( I, Half ) -> AnyHorizontal
        ( I, ThreeQuarters ) -> AnyVertical
        ( I, _ ) -> curRotation

        ( Diagonal, Original ) -> AnyHorizontal
        ( Diagonal, Quarter ) -> AnyVertical
        ( Diagonal, Half ) -> AnyHorizontal
        ( Diagonal, ThreeQuarters ) -> AnyVertical
        ( Diagonal, _ ) -> curRotation

        ( L, _ ) -> curRotation


next : Rotation -> Rotation
next rotation =
    case rotation of
        Original -> Quarter
        Quarter -> Half
        Half -> ThreeQuarters
        ThreeQuarters -> Original
        AnyHorizontal -> AnyVertical
        AnyVertical -> AnyHorizontal
        AnyQuarter -> AnyQuarter



to : Direction -> Rotation -> Rotation
to dir rotation =
    case dir of
        D.N -> rotation |> next
        D.E -> rotation |> next |> next
        D.S -> rotation |> next |> next |> next
        D.W -> rotation
        _ -> rotation


toQuarter : Rotation -> Rotation
toQuarter rotation =
    case rotation of
        Original -> Original
        Quarter -> Quarter
        Half -> Half
        ThreeQuarters -> ThreeQuarters
        AnyHorizontal -> Quarter
        AnyVertical -> Original
        AnyQuarter -> Original


toString : Rotation -> String
toString rotation =
    case rotation of
        Original -> "0°"
        Quarter -> "90°"
        Half -> "180°"
        ThreeQuarters -> "270°"
        AnyHorizontal -> "90°|270°"
        AnyVertical -> "0°|180°"
        AnyQuarter -> "Any"


toId : Rotation -> RotationId
toId rotation =
    case rotation of
        Original -> 0
        Quarter -> 1
        Half -> 2
        ThreeQuarters -> 3
        AnyHorizontal -> 4
        AnyVertical -> 5
        AnyQuarter -> 6


fromId : RotationId -> Rotation
fromId id =
    case id of
        0 -> Original
        1 -> Quarter
        2 -> Half
        3 -> ThreeQuarters
        4 -> AnyHorizontal
        5 -> AnyVertical
        6 -> AnyQuarter
        _ -> Original


toAngle : Rotation -> Float
toAngle rotation =
    case rotation |> toQuarter of
        Original -> 0
        Quarter -> 90
        Half -> 180
        ThreeQuarters -> 270
        _ -> 0
