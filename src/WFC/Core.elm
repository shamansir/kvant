module WFC.Core exposing
    ( .. )


import WFC.Plane exposing (..)


type WFC fmt = WFC (fmt -> fmt)


string : WFC String
string = WFC identity
