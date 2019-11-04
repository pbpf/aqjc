#lang scribble/base
 @(require pict
          pict/tree-layout)
@title[#:tag "yxgf" #:version "v0.1.1" "运行规范"]
@(naive-layered(tree-layout #:pict (text "运行规范")
                                           (tree-edge  (tree-layout #:pict (text "上级指示")))
                                           (tree-edge  (tree-layout #:pict (text "工作流程")))
                                           (tree-edge  (tree-layout #:pict (text "建设标准")))
                                           (tree-edge  (tree-layout #:pict (text "岗位职责")))
                                           (tree-edge  (tree-layout #:pict (text "监察单")))
                                           (tree-edge  (tree-layout #:pict (text "法规依据")))
                                           (tree-edge  (tree-layout #:pict (text "在职学习")))
                                           (tree-edge  (tree-layout #:pict (text "岗位认定")))
                                           (tree-edge  (tree-layout #:pict (text "奖惩制度")))
                                           )
               #:x-spacing 10
                    #:y-spacing 30)
