(ns com.vadelabs.code.interface
  (:require
   [com.vadelabs.code.core :as vm.code]
   [com.vadelabs.code.builder :as vm.builder]))

(def make-builder vm.builder/make-builder)
(def instructions vm.builder/instructions)
(def push vm.builder/push)
(def labels vm.builder/labels)
(def push-data vm.builder/push-data)
(def add-label vm.builder/add-label)
(def builder-data vm.builder/data)

(def make-code vm.code/make-code)
(def code-symbols vm.code/symbols)
(def code vm.code/code)
(def code-data vm.code/data)
(def code-labels vm.code/labels)
(def code-label-ip vm.code/label-ip)
