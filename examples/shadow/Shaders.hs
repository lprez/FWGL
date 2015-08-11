{-# LANGUAGE DataKinds, RebindableSyntax, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, GADTs #-}

module Shaders (
        vertexShader,
        fragmentShader,
        Uniforms,
        Attributes,
        depthVertexShader,
        depthFragmentShader,
        DepthUniforms,
        DepthAttributes,
        LightView3,
        LightPos3,
        ShadowMap
) where

import FWGL.Shader
import FWGL.Shader.Default3D hiding (vertexShader, fragmentShader, Uniforms)
import qualified FWGL.Vector

type Uniforms = '[ Transform3, View3, LightView3, Texture2
                 , ShadowMap, LightPos3 ]
type DepthUniforms = '[ Transform3, LightView3 ]
type DepthAttributes = Attributes

newtype LightView3 = LightView3 Mat4
        deriving (Typeable, ShaderType, UniformCPU CMat4)

newtype LightPos3 = LightPos3 Vec3
        deriving (Typeable, ShaderType, UniformCPU CVec3)

newtype ShadowMap = ShadowMap Sampler2D
        deriving (Typeable, ShaderType, UniformCPU CSampler2D)

newtype ShadowCoord = ShadowCoord Vec4
        deriving (Typeable, ShaderType, AttributeCPU CVec4)

newtype Depth = Depth Float
        deriving (Typeable, ShaderType, AttributeCPU CFloat)

vertexShader :: VertexShader '[ Transform3, View3, LightView3 ] Attributes
                             '[ Position3, UV, Normal3, ShadowCoord ]
vertexShader (Transform3 modelMatrix :- View3 view :- LightView3 lightView :- N)
             (Position3 (Vec3 x y z) :- uv@(UV _) :-
              Normal3 (Vec3 nx ny nz) :- N) =
             let pos4 = Vec4 x y z 1.0
                 worldPos@(Vec4 wx wy wz _) = store $ modelMatrix * pos4
                 scaleBias = Mat4 (Vec4 0.5   0    0   0.5)
                                  (Vec4  0   0.5   0   0.5)
                                  (Vec4  0    0   0.5  0.5)
                                  (Vec4  0    0    0    1 )
                 viewPos = view * worldPos
                 spos = scaleBias * lightView * worldPos
                 Vec4 nx' ny' nz' _ = modelMatrix * Vec4 nx ny nz 0
             in Vertex viewPos :- Position3 (Vec3 wx wy wz) :- uv :-
                Normal3 (Vec3 nx' ny' nz') :- ShadowCoord spos :- N

fragmentShader :: FragmentShader '[ Texture2, ShadowMap, LightPos3 ]
                                 '[ Position3, UV, Normal3, ShadowCoord ]
fragmentShader (Texture2 tex :- ShadowMap shadowMap :-
                LightPos3 lightPos :- N)
               (Position3 wpos :- UV (Vec2 s t) :- Normal3 norm :-
                ShadowCoord (Vec4 sx sy sz sw) :- N) =
                let -- UV mapping
                    Vec4 tr tg tb ta = store . texture2D tex $ Vec2 s (1 - t)

                    -- Light
                    ambCol = Vec3 0.3 0.3 0.1
                    lightVec = store $ wpos - lightPos
                    dist = store $ length lightVec
                    lightDir = store $ normalize lightVec
                    diffFac = store $ 0.7 * dot (normalize norm) (- lightDir)
                    diffCol = if diffFac > 0
                              then Vec3 diffFac diffFac diffFac
                              else Vec3 0 0 0
                    attenuation = 1 + 0.004 * dist + 0.0001 * dist * dist
                    Vec3 lcr lcg lcb = store $ (ambCol + diffCol) / attenuation
                    lightCol = Vec4 lcr lcg lcb 1
                    
                    -- Shadow mapping
                    stepDepth v = let Vec4 depth _ _ _ = texture2D shadowMap v
                                  in step (pz - 0.0015) depth

                    Vec3 px py pz = store $ Vec3 sx sy sz / sw
                    tsize = store $ Vec2 (1 / 1024) (1 / 1024)

                    shadowLerp :: Vec2 -> Float
                    shadowLerp uv =
                        let tuv = store $ uv / tsize + Vec2 0.5 0.5
                            Vec2 fx fy = store . fract $ tuv
                            cuv = store $ floor tuv * tsize
                            lb = stepDepth $ cuv + tsize * Vec2 0 0
                            rb = stepDepth $ cuv + tsize * Vec2 1 0
                            lt = stepDepth $ cuv + tsize * Vec2 0 1
                            rt = stepDepth $ cuv + tsize * Vec2 1 1
                        in mix (mix lb lt fy) (mix rb rt fy) fx

                    shadow = (+ 0.5) . (/ 50) . loop 5 0 $
                                \x s -> flip (,) false . (+ s) . loop 5 0 $
                                        \y s' -> 
                                                let uv = Vec2 px py + 
                                                         Vec2 (x - 2) (y - 2)
                                                                * tsize
                                                in (s' + shadowLerp uv, false)
                in Fragment (Vec4 (shadow * tr)
                                  (shadow * tg)
                                  (shadow * tb)
                                  ta * lightCol) :- N

depthVertexShader :: VertexShader DepthUniforms DepthAttributes '[]
depthVertexShader (Transform3 modelMatrix :- LightView3 viewMatrix :- N)
                  (Position3 (Vec3 x y z) :- _) =
                  Vertex (viewMatrix * modelMatrix * Vec4 x y z 1.0) :- N

depthFragmentShader :: FragmentShader '[] '[]
depthFragmentShader N N = Fragment fragColor :- N
