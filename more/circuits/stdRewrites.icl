// This module contains the standard rewrites (which always appear on the menu) as well as some
// information used in displaying the box-reduction rewrite (which is not stored as a rewrite).

implementation module stdRewrites

import StdEnv
import rewriteDefs, circuitDefs

tensorReduct :: Rewrite
tensorReduct
 = {rewriteName = "Tensor reduction",
    liveWireID = 2,
    leftComp = {spec = TensorI,
                id = 0,
                inputs = [{cWireID = 0, cWireType = (Free (Var 7))}, {cWireID = 1, cWireType = (Free (Var 8))}],
                outputs = [{cWireID = 2, cWireType = (Free (Product ((Var 7),(Var 8))))}],
                pos = PT (157,88)
               },
    leftTop = [{spec = StartTerminal,
                id = 3,
                inputs = [],
                outputs = [{cWireID = 1, cWireType = (Free (Var 8))}],
                pos = PT (175,37)
               },
               {spec = StartTerminal,
                id = 2,
                inputs = [],
                outputs = [{cWireID = 0, cWireType = (Free (Var 7))}],
                pos = PT (139,37)
               }
              ],
    leftMid = [],
    leftBot = [{spec = TensorE,
                id = 1,
                inputs = [{cWireID = 2, cWireType = (Free (Product ((Var 7),(Var 8))))}],
                outputs = [{cWireID = 3, cWireType = (Free (Var 7))}, {cWireID = 4, cWireType = (Free (Var 8))}],
                pos = PT (161,166)
               },
               {spec = EndTerminal,
                id = 4,
                inputs = [{cWireID = 3, cWireType = (Free (Var 7))}],
                outputs = [],
                pos = PT (144,225)
               },
               {spec = EndTerminal,
                id = 5,
                inputs = [{cWireID = 4, cWireType = (Free (Var 8))}],
                outputs = [],
                pos = PT (175,220)
               }
              ],
    rightSide = [],
    oldRightSide = [{spec = StartTerminal,
                     id = 0,
                     inputs = [],
                     outputs = [{cWireID = 0, cWireType = (Free (Var 1))}],
                     pos = PT (139,37)
                    },
                    {spec = StartTerminal,
                     id = 1,
                     inputs = [],
                     outputs = [{cWireID = 1, cWireType = (Free (Var 2))}],
                     pos = PT (175,37)
                    },
                    {spec = EndTerminal,
                     id = 2,
                     inputs = [{cWireID = 0, cWireType = (Free (Var 1))}],
                     outputs = [],
                     pos = PT (139,225)
                    },
                    {spec = EndTerminal,
                     id = 3,
                     inputs = [{cWireID = 1, cWireType = (Free (Var 2))}],
                     outputs = [],
                     pos = PT (175,225)
                    }
                   ],
    ruleRect = ((0,0),(0,0)),
    inConnects = [],
    outConnects = [],
    connectedPairs = [(1,4,Free (Var 8)),(0,3,Free (Var 7))],
    brokenWireConnect = {cWireID = (-1), cWireType = Free Unit}
   }

sumReduct :: Rewrite
sumReduct
 = {rewriteName = "Sum reduction",
    liveWireID = 2,
    leftComp = {spec = SumI,
                id = 0,
                inputs = [{cWireID = 0, cWireType = (Free (Var 7))}, {cWireID = 1, cWireType = (Free (Var 8))}],
                outputs = [{cWireID = 2, cWireType = (Free (Sum ((Var 7),(Var 8))))}],
                pos = PT (192,83)
               },
    leftTop = [{spec = StartTerminal,
                id = 3,
                inputs = [],
                outputs = [{cWireID = 1, cWireType = (Free (Var 8))}],
                pos = PT (214,38)
               },
               {spec = StartTerminal,
                id = 2,
                inputs = [],
                outputs = [{cWireID = 0, cWireType = (Free (Var 7))}],
                pos = PT (176,37)
               }
              ],
    leftMid = [],
    leftBot = [{spec = SumE,
                id = 1,
                inputs = [{cWireID = 2, cWireType = (Free (Sum ((Var 7),(Var 8))))}],
                outputs = [{cWireID = 3, cWireType = (Free (Var 7))}, {cWireID = 4, cWireType = (Free (Var 8))}],
                pos = PT (191,167)
               },
               {spec = EndTerminal,
                id = 4,
                inputs = [{cWireID = 3, cWireType = (Free (Var 7))}],
                outputs = [],
                pos = PT (173,218)
               },
               {spec = EndTerminal,
                id = 5,
                inputs = [{cWireID = 4, cWireType = (Free (Var 8))}],
                outputs = [],
                pos = PT (209,216)
               }
              ],
    rightSide = [],
    oldRightSide = [{spec = StartTerminal,
                     id = 0,
                     inputs = [],
                     outputs = [{cWireID = 0, cWireType = (Free (Var 1))}],
                     pos = PT (176,37)
                    },
                    {spec = StartTerminal,
                     id = 1,
                     inputs = [],
                     outputs = [{cWireID = 1, cWireType = (Free (Var 2))}],
                     pos = PT (214,37)
                    },
                    {spec = EndTerminal,
                     id = 2,
                     inputs = [{cWireID = 0, cWireType = (Free (Var 1))}],
                     outputs = [],
                     pos = PT (176,218)
                    },
                    {spec = EndTerminal,
                     id = 3,
                     inputs = [{cWireID = 1, cWireType = (Free (Var 2))}],
                     outputs = [],
                     pos = PT (214,218)
                    }
                   ],
    ruleRect = ((0,0),(0,0)),
    inConnects = [],
    outConnects = [],
    connectedPairs = [(1,4,Free (Var 8)),(0,3,Free (Var 7))],
    brokenWireConnect = {cWireID = (-1), cWireType = Free Unit}
   }

unitReductLeft :: Rewrite
unitReductLeft
  = {rewriteName = "Unit reduction (left)",
     liveWireID = 1,
     leftComp = {spec = UnitEL,
                 id = 0,
                 inputs = [{cWireID = 2, cWireType = (Free Unit)}, {cWireID = 0, cWireType = (Free (Var 3))}],
                 outputs = [{cWireID = 1, cWireType = (Free (Var 3))}],
                 pos = PT (158,168)
                },
     leftTop = [{spec = StartTerminal,
                 id = 2,
                 inputs = [],
                 outputs = [{cWireID = 0, cWireType = (Free (Var 3))}],
                 pos = PT (165,72)
                },
                {spec = UnitI,
                 id = 1,
                 inputs = [],
                 outputs = [{cWireID = 2, cWireType = (Free Unit)}],
                 pos = PT (145,88)
                }
               ],
     leftMid = [],
     leftBot = [{spec = EndTerminal,
                 id = 3,
                 inputs = [{cWireID = 1, cWireType = (Free (Var 3))}],
                 outputs = [],
                 pos = PT (159,240)
                }
               ],
     rightSide = [],
     oldRightSide = [{spec = StartTerminal,
                      id = 0,
                      inputs = [],
                      outputs = [{cWireID = 0, cWireType = (Free (Var 1))}],
                      pos = PT (165,72)
                     },
                     {spec = EndTerminal,
                      id = 1,
                      inputs = [{cWireID = 0, cWireType = (Free (Var 1))}],
                      outputs = [],
                      pos = PT (165,240)
                     }
                    ],
     ruleRect = ((0,0),(0,0)),
     inConnects = [],
     outConnects = [],
     connectedPairs = [(0,1,Free (Var 3))],
     brokenWireConnect = {cWireID = (-1), cWireType = Free Unit}
    }

unitReductRight :: Rewrite
unitReductRight
  = {rewriteName = "Unit reduction (right)",
     liveWireID = 1,
     leftComp = {spec = UnitER,
                 id = 0,
                 inputs = [{cWireID = 0, cWireType = (Free (Var 3))}, {cWireID = 2, cWireType = (Free Unit)}],
                 outputs = [{cWireID = 1, cWireType = (Free (Var 3))}],
                 pos = PT (162,149)
                },
     leftTop = [{spec = StartTerminal,
                 id = 2,
                 inputs = [],
                 outputs = [{cWireID = 0, cWireType = (Free (Var 3))}],
                 pos = PT (153,50)
                },
                {spec = UnitI,
                 id = 1,
                 inputs = [],
                 outputs = [{cWireID = 2, cWireType = (Free Unit)}],
                 pos = PT (176,73)
                }
               ],
     leftMid = [],
     leftBot = [{spec = EndTerminal,
                 id = 3,
                 inputs = [{cWireID = 1, cWireType = (Free (Var 3))}],
                 outputs = [],
                 pos = PT (165,225)
                }
               ],
     rightSide = [],
     oldRightSide = [{spec = StartTerminal,
                      id = 0,
                      inputs = [],
                      outputs = [{cWireID = 0, cWireType = (Free (Var 1))}],
                      pos = PT (153,50)
                     },
                     {spec = EndTerminal,
                      id = 1,
                      inputs = [{cWireID = 0, cWireType = (Free (Var 1))}],
                      outputs = [],
                      pos = PT (153,225)
                     }
                    ],
     ruleRect = ((0,0),(0,0)),
     inConnects = [],
     outConnects = [],
     connectedPairs = [(0,1,Free (Var 3))],
     brokenWireConnect = {cWireID = (-1), cWireType = Free Unit}
    }

counitReductLeft :: Rewrite
counitReductLeft
  = {rewriteName = "Counit reduction (left)",
     liveWireID = 1,
     leftComp = {spec = CounitIL,
                 id = 0,
                 inputs = [{cWireID = 0, cWireType = (Free (Var 3))}],
                 outputs = [{cWireID = 2, cWireType = (Free Counit)}, {cWireID = 1, cWireType = (Free (Var 3))}],
                 pos = PT (182,128)
                },
     leftTop = [{spec = StartTerminal,
                 id = 2,
                 inputs = [],
                 outputs = [{cWireID = 0, cWireType = (Free (Var 3))}],
                 pos = PT (183,77)
                }
               ],
     leftMid = [],
     leftBot = [{spec = EndTerminal,
                 id = 3,
                 inputs = [{cWireID = 1, cWireType = (Free (Var 3))}],
                 outputs = [],
                 pos = PT (188,244)
                },
                {spec = CounitE,
                 id = 1,
                 inputs = [{cWireID = 2, cWireType = (Free Counit)}],
                 outputs = [],
                 pos = PT (169,211)
                }
               ],
     rightSide = [],
     oldRightSide = [{spec = StartTerminal,
                      id = 0,
                      inputs = [],
                      outputs = [{cWireID = 0, cWireType = (Free (Var 1))}],
                      pos = PT (183,77)
                     },
                     {spec = EndTerminal,
                      id = 1,
                      inputs = [{cWireID = 0, cWireType = (Free (Var 1))}],
                      outputs = [],
                      pos = PT (183,244)
                     }
                    ],
     ruleRect = ((0,0),(0,0)),
     inConnects = [],
     outConnects = [],
     connectedPairs = [(0,1,Free (Var 3))],
     brokenWireConnect = {cWireID = (-1), cWireType = Free Unit}
    }

counitReductRight :: Rewrite
counitReductRight
  = {rewriteName = "Counit reduction (right)",
     liveWireID = 1,
     leftComp = {spec = CounitIR,
                 id = 0,
                 inputs = [{cWireID = 0, cWireType = (Free (Var 3))}],
                 outputs = [{cWireID = 1, cWireType = (Free (Var 3))}, {cWireID = 2, cWireType = (Free Counit)}],
                 pos = PT (192,115)
                },
     leftTop = [{spec = StartTerminal,
                 id = 2,
                 inputs = [],
                 outputs = [{cWireID = 0, cWireType = (Free (Var 3))}],
                 pos = PT (189,52)
                }
               ],
     leftMid = [],
     leftBot = [{spec = EndTerminal,
                 id = 3,
                 inputs = [{cWireID = 1, cWireType = (Free (Var 3))}],
                 outputs = [],
                 pos = PT (181,255)
                },
                {spec = CounitE,
                 id = 1,
                 inputs = [{cWireID = 2, cWireType = (Free Counit)}],
                 outputs = [],
                 pos = PT (203,199)
                }
               ],
     rightSide = [],
     oldRightSide = [{spec = StartTerminal,
                      id = 0,
                      inputs = [],
                      outputs = [{cWireID = 0, cWireType = (Free (Var 1))}],
                      pos = PT (189,52)
                     },
                     {spec = EndTerminal,
                      id = 1,
                      inputs = [{cWireID = 0, cWireType = (Free (Var 1))}],
                      outputs = [],
                      pos = PT (189,255)
                     }
                    ],
     ruleRect = ((0,0),(0,0)),
     inConnects = [],
     outConnects = [],
     connectedPairs = [(0,1,Free (Var 3))],
     brokenWireConnect = {cWireID = (-1), cWireType = Free Unit}
    }

tensorExp :: Rewrite
tensorExp
  = {rewriteName = "Tensor expansion",
     liveWireID = 0,
     leftComp = {spec = StartTerminal,
                 id = 0,
                 inputs = [],
                 outputs = [{cWireID = 0, cWireType = (User (Product ((Var 1),(Var 2))))}],
                 pos = PT (139,61)
                },
     leftTop = [],
     leftMid = [],
     leftBot = [{spec = EndTerminal,
                 id = 1,
                 inputs = [{cWireID = 0, cWireType = (User (Product ((Var 1),(Var 2))))}],
                 outputs = [],
                 pos = PT (144,190)
                }
               ],
     rightSide = [{spec = TensorE,
                   id = 0,
                   inputs = [{cWireID = 0, cWireType = (Free (Product ((Var 3),(Var 4))))}],
                   outputs = [{cWireID = 2, cWireType = (Free (Var 3))},
                              {cWireID = 3, cWireType = (Free (Var 4))}
                             ],
                   pos = PT (68,114)
                  },
                  {spec = TensorI,
                   id = 1,
                   inputs = [{cWireID = 2, cWireType = (Free (Var 3))},
                             {cWireID = 3, cWireType = (Free (Var 4))}
                            ],
                   outputs = [{cWireID = 4, cWireType = (Free (Product ((Var 3),(Var 4))))}],
                   pos = PT (68,142)
                  }
                 ],
     oldRightSide = [{spec = StartTerminal,
                      id = 0,
                      inputs = [],
                      outputs = [{cWireID = 0, cWireType = (Free (Product ((Var 3),(Var 4))))}],
                      pos = PT (68,61)
                     },
                     {spec = TensorE,
                      id = 1,
                      inputs = [{cWireID = 0, cWireType = (Free (Product ((Var 3),(Var 4))))}],
                      outputs = [{cWireID = 2, cWireType = (Free (Var 3))},
                                 {cWireID = 3, cWireType = (Free (Var 4))}
                                ],
                      pos = PT (68,114)
                     },
                     {spec = TensorI,
                      id = 2,
                      inputs = [{cWireID = 2, cWireType = (Free (Var 3))},
                                {cWireID = 3, cWireType = (Free (Var 4))}
                               ],
                      outputs = [{cWireID = 4, cWireType = (Free (Product ((Var 3),(Var 4))))}],
                      pos = PT (68,142)
                     },
                     {spec = EndTerminal,
                      id = 3,
                      inputs = [{cWireID = 4, cWireType = (Free (Product ((Var 3),(Var 4))))}],
                      outputs = [],
                      pos = PT (68,190)
                     }
                    ],
     ruleRect = ((62,108),(74,148)),
     inConnects = [{cWireID = 0, cWireType = (Free (Product ((Var 3),(Var 4))))}],
     outConnects = [],
     connectedPairs = [],
     brokenWireConnect = {cWireID = 4, cWireType = (Free (Product ((Var 3),(Var 4))))}
    }

sumExp :: Rewrite
sumExp
  = {rewriteName = "Sum expansion",
     liveWireID = 0,
     leftComp = {spec = StartTerminal,
                 id = 0,
                 inputs = [],
                 outputs = [{cWireID = 0, cWireType = (User (Sum ((Var 1),(Var 2))))}],
                 pos = PT (139,61)
                },
     leftTop = [],
     leftMid = [],
     leftBot = [{spec = EndTerminal,
                 id = 1,
                 inputs = [{cWireID = 0, cWireType = (User (Sum ((Var 1),(Var 2))))}],
                 outputs = [],
                 pos = PT (144,190)
                }
               ],
     rightSide = [{spec = SumE,
                   id = 0,
                   inputs = [{cWireID = 0, cWireType = (Free (Sum ((Var 3),(Var 4))))}],
                   outputs = [{cWireID = 2, cWireType = (Free (Var 3))},
                              {cWireID = 3, cWireType = (Free (Var 4))}
                             ],
                   pos = PT (108,96)
                  },
                  {spec = SumI,
                   id = 1,
                   inputs = [{cWireID = 2, cWireType = (Free (Var 3))},
                             {cWireID = 3, cWireType = (Free (Var 4))}
                            ],
                   outputs = [{cWireID = 4, cWireType = (Free (Sum ((Var 3),(Var 4))))}],
                   pos = PT (108,124)
                  }
                 ],
     oldRightSide = [{spec = StartTerminal,
                      id = 0,
                      inputs = [],
                      outputs = [{cWireID = 0, cWireType = (Free (Sum ((Var 3),(Var 4))))}],
                      pos = PT (108,61)
                     },
                     {spec = SumE,
                      id = 1,
                      inputs = [{cWireID = 0, cWireType = (Free (Sum ((Var 3),(Var 4))))}],
                      outputs = [{cWireID = 2, cWireType = (Free (Var 3))},
                                 {cWireID = 3, cWireType = (Free (Var 4))}
                                ],
                      pos = PT (108,96)
                     },
                     {spec = SumI,
                      id = 2,
                      inputs = [{cWireID = 2, cWireType = (Free (Var 3))},
                                {cWireID = 3, cWireType = (Free (Var 4))}
                               ],
                      outputs = [{cWireID = 4, cWireType = (Free (Sum ((Var 3),(Var 4))))}],
                      pos = PT (108,124)
                     },
                     {spec = EndTerminal,
                      id = 3,
                      inputs = [{cWireID = 4, cWireType = (Free (Sum ((Var 3),(Var 4))))}],
                      outputs = [],
                      pos = PT (108,190)
                     }
                    ],
     ruleRect = ((102,90),(114,130)),
     inConnects = [{cWireID = 0, cWireType = (Free (Sum ((Var 3),(Var 4))))}],
     outConnects = [],
     connectedPairs = [],
     brokenWireConnect = {cWireID = 4, cWireType = (Free (Sum ((Var 3),(Var 4))))}
    }

unitExpLeft :: Rewrite
unitExpLeft
  = {rewriteName = "Unit expansion (left)",
     liveWireID = 0,
     leftComp = {spec = StartTerminal,
                 id = 0,
                 inputs = [],
                 outputs = [{cWireID = 0, cWireType = (User Unit)}],
                 pos = PT (139,61)
                },
     leftTop = [],
     leftMid = [],
     leftBot = [{spec = EndTerminal,
                 id = 1,
                 inputs = [{cWireID = 0, cWireType = (User Unit)}],
                 outputs = [],
                 pos = PT (144,190)
                }
               ],
     rightSide = [{spec = UnitI,
                   id = 2,
                   inputs = [],
                   outputs = [{cWireID = 2, cWireType = (Free Unit)}],
                   pos = PT (154,113)
                  },
                  {spec = UnitEL,
                   id = 1,
                   inputs = [{cWireID = 0, cWireType = (Free Unit)}, {cWireID = 2, cWireType = (Free Unit)}],
                   outputs = [{cWireID = 3, cWireType = (Free Unit)}],
                   pos = PT (150,147)
                  }
                 ],
     oldRightSide = [{spec = StartTerminal,
                      id = 0,
                      inputs = [],
                      outputs = [{cWireID = 0, cWireType = (Free Unit)}],
                      pos = PT (139,61)
                     },
                     {spec = UnitI,
                      id = 2,
                      inputs = [],
                      outputs = [{cWireID = 2, cWireType = (Free Unit)}],
                      pos = PT (154,113)
                     },
                     {spec = UnitEL,
                      id = 1,
                      inputs = [{cWireID = 0, cWireType = (Free Unit)}, {cWireID = 2, cWireType = (Free Unit)}],
                      outputs = [{cWireID = 3, cWireType = (Free Unit)}],
                      pos = PT (150,147)
                     },
                     {spec = EndTerminal,
                      id = 3,
                      inputs = [{cWireID = 3, cWireType = (Free Unit)}],
                      outputs = [],
                      pos = PT (150,190)
                     }
                    ],
     ruleRect = ((132,107),(160,152)),
     inConnects = [{cWireID = 0, cWireType = (Free Unit)}],
     outConnects = [],
     connectedPairs = [],
     brokenWireConnect = {cWireID = 3, cWireType = (Free Unit)}
    }

unitExpRight :: Rewrite
unitExpRight
  = {rewriteName = "Unit expansion (right)",
     liveWireID = 0,
     leftComp = {spec = StartTerminal,
                 id = 0,
                 inputs = [],
                 outputs = [{cWireID = 0, cWireType = (User Unit)}],
                 pos = PT (139,61)
                },
     leftTop = [],
     leftMid = [],
     leftBot = [{spec = EndTerminal,
                 id = 1,
                 inputs = [{cWireID = 0, cWireType = (User Unit)}],
                 outputs = [],
                 pos = PT (144,190)
                }
               ],
     rightSide = [{spec = UnitI,
                   id = 1,
                   inputs = [],
                   outputs = [{cWireID = 1, cWireType = (Free Unit)}],
                   pos = PT (107,113)
                  },
                  {spec = UnitER,
                   id = 0,
                   inputs = [{cWireID = 1, cWireType = (Free Unit)}, {cWireID = 0, cWireType = (Free Unit)}],
                   outputs = [{cWireID = 2, cWireType = (Free Unit)}],
                   pos = PT (110,146)
                  }
                 ],
     oldRightSide = [{spec = StartTerminal,
                      id = 0,
                      inputs = [],
                      outputs = [{cWireID = 0, cWireType = (Free Unit)}],
                      pos = PT (139,61)
                     },
                     {spec = UnitI,
                      id = 1,
                      inputs = [],
                      outputs = [{cWireID = 1, cWireType = (Free Unit)}],
                      pos = PT (107,113)
                     },
                     {spec = UnitER,
                      id = 2,
                      inputs = [{cWireID = 1, cWireType = (Free Unit)}, {cWireID = 0, cWireType = (Free Unit)}],
                      outputs = [{cWireID = 2, cWireType = (Free Unit)}],
                      pos = PT (110,146)
                     },
                     {spec = EndTerminal,
                      id = 3,
                      inputs = [{cWireID = 2, cWireType = (Free Unit)}],
                      outputs = [],
                      pos = PT (110,190)
                     }
                    ],
     ruleRect = ((101,107),(128,151)),
     inConnects = [{cWireID = 0, cWireType = (Free Unit)}],
     outConnects = [],
     connectedPairs = [],
     brokenWireConnect = {cWireID = 2, cWireType = (Free Unit)}
    }

counitExpLeft :: Rewrite
counitExpLeft
  = {rewriteName = "Counit expansion (left)",
     liveWireID = 0,
     leftComp = {spec = StartTerminal,
                 id = 0,
                 inputs = [],
                 outputs = [{cWireID = 0, cWireType = (User Counit)}],
                 pos = PT (139,61)
                },
     leftTop = [],
     leftMid = [],
     leftBot = [{spec = EndTerminal,
                 id = 1,
                 inputs = [{cWireID = 0, cWireType = (User Counit)}],
                 outputs = [],
                 pos = PT (144,190)
                }
               ],
     rightSide = [{spec = CounitIL,
                   id = 0,
                   inputs = [{cWireID = 0, cWireType = (Free Counit)}],
                   outputs = [{cWireID = 3, cWireType = (Free Counit)}, {cWireID = 2, cWireType = (Free Counit)}],
                   pos = PT (109,126)
                  },
                  {spec = CounitE,
                   id = 1,
                   inputs = [{cWireID = 2, cWireType = (Free Counit)}],
                   outputs = [],
                   pos = PT (110,164)
                  }
                 ],
     oldRightSide = [{spec = StartTerminal,
                      id = 0,
                      inputs = [],
                      outputs = [{cWireID = 0, cWireType = (Free Counit)}],
                      pos = PT (109,61)
                     },
                     {spec = CounitIL,
                      id = 1,
                      inputs = [{cWireID = 0, cWireType = (Free Counit)}],
                      outputs = [{cWireID = 3, cWireType = (Free Counit)}, {cWireID = 2, cWireType = (Free Counit)}],
                      pos = PT (109,126)
                     },
                     {spec = CounitE,
                      id = 2,
                      inputs = [{cWireID = 2, cWireType = (Free Counit)}],
                      outputs = [],
                      pos = PT (110,164)
                     },
                     {spec = EndTerminal,
                      id = 3,
                      inputs = [{cWireID = 3, cWireType = (Free Counit)}],
                      outputs = [],
                      pos = PT (100,190)
                     }
                    ],
     ruleRect = ((91,121),(116,170)),
     inConnects = [{cWireID = 0, cWireType = (Free Counit)}],
     outConnects = [],
     connectedPairs = [],
     brokenWireConnect = {cWireID = 3, cWireType = (Free Counit)}
    }

counitExpRight :: Rewrite
counitExpRight
  = {rewriteName = "Counit expansion (right)",
     liveWireID = 0,
     leftComp = {spec = StartTerminal,
                 id = 0,
                 inputs = [],
                 outputs = [{cWireID = 0, cWireType = (User Counit)}],
                 pos = PT (139,61)
                },
     leftTop = [],
     leftMid = [],
     leftBot = [{spec = EndTerminal,
                 id = 1,
                 inputs = [{cWireID = 0, cWireType = (User Counit)}],
                 outputs = [],
                 pos = PT (144,190)
                }
               ],
     rightSide = [{spec = CounitIR,
                   id = 0,
                   inputs = [{cWireID = 0, cWireType = (Free Counit)}],
                   outputs = [{cWireID = 2, cWireType = (Free Counit)}, {cWireID = 3, cWireType = (Free Counit)}],
                   pos = PT (132,147)
                  },
                  {spec = CounitE,
                   id = 1,
                   inputs = [{cWireID = 2, cWireType = (Free Counit)}],
                   outputs = [],
                   pos = PT (132,185)
                  }
                 ],
     oldRightSide = [{spec = StartTerminal,
                      id = 0,
                      inputs = [],
                      outputs = [{cWireID = 0, cWireType = (Free Counit)}],
                      pos = PT (132,61)
                     },
                     {spec = CounitIR,
                      id = 1,
                      inputs = [{cWireID = 0, cWireType = (Free Counit)}],
                      outputs = [{cWireID = 2, cWireType = (Free Counit)}, {cWireID = 3, cWireType = (Free Counit)}],
                      pos = PT (132,147)
                     },
                     {spec = CounitE,
                      id = 2,
                      inputs = [{cWireID = 2, cWireType = (Free Counit)}],
                      outputs = [],
                      pos = PT (132,185)
                     },
                     {spec = EndTerminal,
                      id = 3,
                      inputs = [{cWireID = 3, cWireType = (Free Counit)}],
                      outputs = [],
                      pos = PT (144,190)
                     }
                    ],
     ruleRect = ((126,142),(150,191)),
     inConnects = [{cWireID = 0, cWireType = (Free Counit)}],
     outConnects = [],
     connectedPairs = [],
     brokenWireConnect = {cWireID = 3, cWireType = (Free Counit)}
    }

boxExp :: Rewrite
boxExp
  = {rewriteName = "Box expansion",
     liveWireID = 0,
     leftComp = {spec = StartTerminal,
                 id = 0,
                 inputs = [],
                 outputs = [{cWireID = 0, cWireType = (User (Then ((Var 1),(Var 2))))}],
                 pos = PT (139,61)
                },
     leftTop = [],
     leftMid = [],
     leftBot = [{spec = EndTerminal,
                 id = 1,
                 inputs = [{cWireID = 0, cWireType = (User (Then ((Var 1),(Var 2))))}],
                 outputs = [],
                 pos = PT (144,190)
                }
               ],
     rightSide = [{spec = Box [{spec = Lolly,
                                id = 1,
                                inputs = [{cWireID = 1, cWireType = (Free (Var 3))},
                                          {cWireID = 0, cWireType = (Free (Then ((Var 3),(Var 2))))}
                                         ],
                                outputs = [{cWireID = 3, cWireType = (Free (Var 2))}],
                                pos = PT (147,122)
                               }
                              ],
                   id = 0,
                   inputs = [{cWireID = 3, cWireType = (Free (Var 2))}],
                   outputs = [{cWireID = 1, cWireType = (Free (Var 3))},
                              {cWireID = 4, cWireType = (Free (Then ((Var 3),(Var 2))))}],
                   pos = RCT ((101,94),(173,160))
                  }
                 ],
     oldRightSide = [{spec = StartTerminal,
                      id = 0,
                      inputs = [],
                      outputs = [{cWireID = 0, cWireType = (Free (Then ((Var 3),(Var 2))))}],
                      pos = PT (160,61)
                     },
                     {spec = Box [{spec = Lolly,
                                   id = 1,
                                   inputs = [{cWireID = 1, cWireType = (Free (Var 3))},
                                             {cWireID = 0, cWireType = (Free (Then ((Var 3),(Var 2))))}
                                            ],
                                   outputs = [{cWireID = 3, cWireType = (Free (Var 2))}],
                                   pos = PT (147,122)
                                  }
                                 ],
                      id = 2,
                      inputs = [{cWireID = 3, cWireType = (Free (Var 2))}],
                      outputs = [{cWireID = 1, cWireType = (Free (Var 3))},
                                 {cWireID = 4, cWireType = (Free (Then ((Var 3),(Var 2))))}],
                      pos = RCT ((101,94),(173,160))
                     },
                     {spec = EndTerminal,
                      id = 3,
                      inputs = [{cWireID = 4, cWireType = (Free (Then ((Var 3),(Var 2))))}],
                      outputs = [],
                      pos = PT (137,190)
                     }
                    ],
     ruleRect = ((101,94),(173,166)),
     inConnects = [{cWireID = 0, cWireType = (Free (Then ((Var 3),(Var 2))))}],
     outConnects = [],
     connectedPairs = [],
     brokenWireConnect = {cWireID = 4, cWireType = (Free (Then ((Var 3),(Var 2))))}
    }

boxReductLiveWireID :: Int
boxReductLiveWireID = 5

boxReductLeftSide :: Circuit
boxReductLeftSide = [{spec    = (Box
                                 [{spec    = (Generic "   " [(Var 0)] [(Var 1)]),
                                   id      = 2,
                                   inputs  = [{cWireID = 2, cWireType = (Free (Var 5))}],
                                   outputs = [{cWireID = 3, cWireType = (Free (Var 8))}],
                                   pos     = (RCT ((115,42),(145,66)))
                                  }
                                 ]
                                ),
                      id      = 1,
                      inputs  = [{cWireID = 3, cWireType = (Free (Var 8))}],
                      outputs = [{cWireID = 2, cWireType = (Free (Var 5))},
                                 {cWireID = 5, cWireType = (Free (Then ((Var 5),(Var 8))))}
                                ],
                      pos     = (RCT ((71,13),(189,95)))
                     },
                     {spec    = StartTerminal,
                             id      = 4,
                      inputs  = [],
                      outputs = [{cWireID = 4, cWireType = (Free (Var 5))}],
                      pos     = (PT (37,19))
                     },
                     {spec    = Lolly,
                      id      = 3,
                      inputs  = [{cWireID = 4, cWireType = (Free (Var 5))},
                                 {cWireID = 5, cWireType = (Free (Then ((Var 5),(Var 8))))}
                                ],
                      outputs = [{cWireID = 6, cWireType = (Free (Var 8))}],
                      pos     = (PT (85,176))
                     },
                     {spec    = EndTerminal,
                      id      = 5,
                      inputs  = [{cWireID = 6, cWireType = (Free (Var 8))}],
                      outputs = [],
                      pos     = (PT (84,263))
                     }
                    ]

boxReductLeftWires :: [Wire]
boxReductLeftWires = [{wireID = 6, wireLine = ((85,188),(84,255)), wireType = (Free (Var 8))},
                      {wireID = 5, wireLine = ((130,107),(95,164)), wireType = (Free (Then ((Var 5),(Var 8))))},
                      {wireID = 4, wireLine = ((37,27),(75,164)), wireType = (Free (Var 5))},
                      {wireID = 3, wireLine = ((130,72),(130,83)), wireType = (Free (Var 8))},
                      {wireID = 2, wireLine = ((130,21),(130,36)), wireType = (Free (Var 5))}
                     ]

boxReductLeftCompID :: Int
boxReductLeftCompID = 6

boxReductLeftNextWireID :: Int
boxReductLeftNextWireID = 7

boxReductLeftNextVar :: Int
boxReductLeftNextVar = 9

boxReductRightSide :: Circuit
boxReductRightSide = [{spec    = StartTerminal,
                       id      = 1,
                       inputs  = [],
                       outputs = [{cWireID = 0, cWireType = (Free (Var 1))}],
                       pos     = (PT (73,25))
                      },
                      {spec    = (Generic "   " [(Var 0)] [(Var 1)]),
                       id      = 0,
                       inputs  = [{cWireID = 0, cWireType = (Free (Var 1))}],
                       outputs = [{cWireID = 1, cWireType = (Free (Var 4))}],
                       pos     = (RCT ((58,77),(88,101)))
                      },
                      {spec    = EndTerminal,
                       id      = 2,
                       inputs  = [{cWireID = 1, cWireType = (Free (Var 4))}],
                       outputs = [],
                       pos     = (PT (73,158))
                      }
                     ]

boxReductRightWires :: [Wire]
boxReductRightWires = [{wireID = 1, wireLine = ((73,107),(73,150)), wireType = (Free (Var 4))},
                       {wireID = 0, wireLine = ((73,33),(73,71)), wireType = (Free (Var 1))}
                      ]

boxReductRightCompID :: Int
boxReductRightCompID = 3

boxReductRightNextWireID :: Int
boxReductRightNextWireID = 2

boxReductRightNextVar :: Int
boxReductRightNextVar = 5
