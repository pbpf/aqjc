#lang scribble/base
@(require pict
          pict/tree-layout)
@title[#:tag "gzjh" #:version "v0.1.1" "工作计划"]
@(naive-layered (tree-layout #:pict (text "工作计划")
                                           (tree-edge  (tree-layout #:pict (text "年度计划")))
                                           (tree-edge  (tree-layout #:pict (text "月计划")))
                                           (tree-edge  (tree-layout #:pict (text "日计划")))
                                           (tree-edge  (tree-layout #:pict (text "专项计划")))
                                           (tree-edge  (tree-layout #:pict (text "在职学习计划")))
                                           (tree-edge  (tree-layout #:pict (text "法规考核计划"))))
                #:x-spacing 10
                    #:y-spacing 30)
