module Flame

open MathNet.Numerics.LinearAlgebra


type setState =
| Opacity of float32 * string

type linkable<'T> =
| Direct     of 'T
| Indirect   of string

type varCode = {
    parameters  : string []
    code        : string
}

type var = {
    weight      : float32
    parameters  : Map<string,float32>
    desc        : varCode
}

type affine = 
| Affine2D      of matrix:Matrix<float32> * offset:Vector<float32>
| Affine3D      of matrix:Matrix<float32> * offset:Vector<float32>

type transformation = {
    affine  : affine option
    vars    : var []
}

and nodeTarget = 
| Traverse  of weight:float32 * target:node
| Enter     of weight:float32 * target:node
| Return    of weight:float32 * state:string
| LeaveTo   of weight:float32 * target:node

and node = {
    transformation      : transformation
    setStates           : setState []
    opacity             : linkable<float32>
    colorIndex          : float32
    colorSpeed          : float32
    mutable targets     : nodeTarget []
    usePointPool        : bool
    mutable continuation: nodeTarget []
}

type color = {
    r : float32
    g : float32
    b : float32
}

type camera =
| Affine of affine
| LookAtCircle  of target:Vector<float32> * radius:float32 * up:Vector<float32>
| LookAtSphere  of target:Vector<float32> * radius:float32 * up:Vector<float32>


type gamut = {
    brightness  : float32
    gamma       : float32
    vibrancy    : float32
}

type flame = {
    states          : Set<string>
    nodes           : Map<string,node>
    transformations : Map<string,transformation>
    camera          : camera
    gamut           : gamut
    palette         : color []
    code            : Map<string,varCode> list
}

type coordinateSystem =
| Cartesian

type interpolation =
| Linear
| Cubic     of s0:float32 * s1:float32
| Quintic   of s0:float32 * s1:float32 * c0:float32 * c1:float32

type affineLink = {
    start : affine
    stop  : affine
    interpolation   : interpolation
    coordinateSystem: coordinateSystem
}

type varLink = {
    start : var
    stop  : var
    interpolation : interpolation
}

type transformationLink = {
    affineLink  : affineLink option
    varLinks    : varLink []
}

type transitionNodeLink = {
    start : node
    stop  : node
    interpolation : interpolation
}

type cameraLink = {
    start : camera
    stop  : camera
    interpolation   : interpolation
    coordinateSystem: coordinateSystem
}

type gamutLink = {
    start : gamut
    stop  : gamut
    interpolation : interpolation
}

type paletteInterpolation =
| RGB
| HSV

type paletteLink = {
    start : color []
    stop  : color []
    interpolation       : interpolation
    paletteInterpolation: paletteInterpolation
}

type flameLink = {
    start               : flame
    stop                : flame
    stateLinks          : (string * string) []
    transformationLinks : transformationLink []
    cameraLink          : cameraLink
    gamutLink           : gamutLink
    paletteLink         : paletteLink
}

type flameTransition = {
    link        : flameLink
    startTime   : float32
    stopTime    : float32
}

type cameraTransition = {
    link        : cameraLink
    startTime   : float32
    stopTime    : float32
}

type animation = {
    flameSpans : flameTransition []
    cameraSpans: cameraTransition []
}

type flam35 = {
    animation : animation option
    flames    : flame []
}