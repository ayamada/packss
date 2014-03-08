<div align="center"><img src="https://github.com/ayamada/packss/raw/master/logo.png" /></div>


# packss

pack/unpack shared-structure


## What is this

This is to support to serialize/deserialize module.


## Usage

~~~
(require '[packss.core :as packss])
(require '[clojure.edn :as edn])

(def ss (let [a (atom 1)]
          (list a a)))
ss => (#<Atom@1bc865a: 1> #<Atom@1bc865a: 1>)
(reset! (first ss) 2)
ss => (#<Atom@1bc865a: 2> #<Atom@1bc865a: 2>)

(def serialized (pr-str (packss/pack ss)))
(def deserialized (packss/unpack (edn/read-str serialized)))
deserialized => (#<Atom@11a6cc4: 2> #<Atom@11a6cc4: 2>)
(reset! (first deserialized) 3)
deserialized => (#<Atom@11a6cc4: 3> #<Atom@11a6cc4: 3>)
~~~


## Can to do

- Can treat shared-structure
    - See [srfi-38](http://srfi.schemers.org/srfi-38/srfi-38.html)
      for what is shared-structure.

- Can `pack` to Clojure's atom, ref, defrecord, Java's array

- Can `pack` your object if you set `mapper`, `unmapper`, and `replacer`

- Can filter/scan all objects in structure


## Cannot to do

- Cannot genuine serialization
    - `pack` generate `vector` of clojure.
      You may use packss with another serialize libraries, for example,
      [edn](http://clojure.github.io/clojure/clojure.edn-api.html),
      [nippy](https://github.com/ptaoussanis/nippy),
      [fressian](https://github.com/clojure/data.fressian),
      [carbonite](https://github.com/sritchie/carbonite),
      etc...

- Cannot `pack` infinity (or cyclic) lazy-seq
    - Because, below expr is false.
        - `(let [r (repeat :a)] (identical? r (rest r)))`


## Glossary

- `pack` / `unpack`
    - `pack` convert from a structure to serializable vector.
    - `unpack` is vice versa.

- `mapping`(`mapper`) / `unmapping`(`unmapper`) / `replacing!`(`replacer`)
    - `mapping` convert from one object in structure to serializable data.
    - `unmapping` is vice versa.
    - `replacing!` support to `unmapping`.
      It changes object's values with side-effect at after `unmapping`.
      It should need to reconstruct shared structure.

- `idx`(`index`)
    - `pack` generate a vector. All entities are numbered.


## License

Copyright c 2014 ayamada

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

