module Flame


type linkable<'T> =
|Direct     of 'T
|Indirect   of state:string

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
|Affine2D of matrix:float32[,] * offset:float32[]
|Affine3D of matrix:float32[,] * offset:float32[]

type transformation = {
    affine  : affine option
    vars    : var []
}

type nodeLink = {
    weight  : float32
    target  : node
}

and node = {
    opacity     : linkable<float32>
    colorIndex  : float32
    colorSpeed  : float32
    targets     : nodeLink []
    usePointPool: bool
    continuation: nodeLink [] option
}

type color = {
    r : float32
    g : float32
    b : float32
}

type camera =
|Affine of matrix:float32[,]

type gamut = {
    gamma       : float32
    brightness  : float32
    vibrancy    : float32
}

type postProcess = {
    doDensityEstimation : bool
    gamut               : gamut
}

type flame = {
    postProcess     : postProcess
    camera          : camera
    nodes           : Map<string,node>
    transformations : Map<string,transformation>
    palette         : color []
    code            : Map<string,varCode>
}