(:source-file
 :pos 0
 :end 21
 :statements ((:expression-statement
               :pos 0
               :end 21
               :expression
               (:call-expression
                :pos 0
                :end 20
                :expression (:property-access-expression
                             :pos 0
                             :end 11
                             :name (:identifier
                                    :pos 8
                                    :end 11
                                    :escaped-text "log")
                             :expression (:identifier
                                          :pos 0
                                          :end 7
                                          :escaped-text
                                          "console"))
                :arguments (:node-array
                            :pos 12
                            :end 26
                            :node-list
                            ((:string-literal
                              :pos 12
                              :end 26
                              :text "hello, world"))))))
 :end-of-file-token (:end-of-file-token
                     :pos 28
                     :end 28)
 :filename "astExplorer.tsx"
 :text "console.log('hello, world');")
