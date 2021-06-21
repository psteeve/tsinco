
(defvar *type-alias-declaration* '(:pos 14
                                   :end 100
                                   :modifiers (:node-array
                                               :pos 129
                                               :end 130
                                               :node-list
                                               ((:export-keyword
                                                 :pos 14
                                                 :end 25)))
                                   :name (:identifier
                                          :pos 30
                                          :end 44
                                          escaped-text "ErrorCallback")
                                   :type (:function-type
                                          :pos 46
                                          :end 99
                                          :parameters (:node-array
                                                       :pos 48
                                                       :end 90
                                                       :node-list
                                                       ((:parameter
                                                         :pos 48
                                                         :end 74
                                                         :name (:identifier
                                                                :pos 48
                                                                :end 55
                                                                :escaped-text "message")
                                                         :type (:type-reference
                                                                :pos 56
                                                                :end 74
                                                                :type-name (:identifier
                                                                            :pos 56
                                                                            :end 74
                                                                            :escaped-text "DiagnosticMessage")))
                                                        (:parameter
                                                         :pos 75
                                                         :end 90
                                                         :name (:identifier
                                                                :pos 75
                                                                :end 82
                                                                :escaped-text "length")
                                                         :type (:number-keyword
                                                                :pos 83
                                                                :end 90)))))))




(defvar *module-delcaratiion* (:module-declaration
                               :pos 0
                               :end 113623
                               :name (:identifier
                                      :pos 9
                                      :end 12
                                      :escaped-text "ts")
                               :body (:module-block
                                      :pos 12
                                      :end 113623
                                      :statements (*type-alias-declaration* *function-declaration-1* *function-declaration-2*
                                                                            *interface-declaration*))))
