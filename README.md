<div align="center"><img src="https://github.com/ayamada/packss/raw/master/logo.png" /></div>


# packss

pack/unpack shared-structure


## What is this

This is to support to serialize/deserialize module.


## Install

- https://clojars.org/packss


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
(def deserialized (packss/unpack (edn/read-string serialized)))
deserialized => (#<Atom@11a6cc4: 2> #<Atom@11a6cc4: 2>)
(reset! (first deserialized) 3)
deserialized => (#<Atom@11a6cc4: 3> #<Atom@11a6cc4: 3>)
~~~


## Can to do

- Can treat shared-structure (including cyclic-structure without lazy-seq)
    - See [srfi-38](http://srfi.schemers.org/srfi-38/srfi-38.html)
      for what is shared-structure.

- Can `pack` to Clojure's map, set, list, vector, seq, coll, **defrecord**,
  **atom**, **ref**, and **Java's array**

- Can `pack` any your objects if you set `mapper`, `unmapper`, and `replacer`

- Can filter/scan all objects in structure


## Cannot to do

- Cannot genuine serialization
    - `pack` generate `vector` of Clojure.
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

- `ext`(`user-ext`)
    - `pack`/`unpack`'s optional arg, additional defines for `pack`/`unpack`.

- `scanner`
    - `pack`'s optional arg, check/substitute individual object.


## FAQ

- I want to `pack` instances of unsupported classes.
    - Use `make-packss-table`. See `test/packss/core_test.clj`.
    - If you want to `pack` class include clojure object recursively,
      you shall use `obj->idx` and `idx->obj`.
      See `built-in-packss-table` in `src/packss/core.clj`.

- `pack` don't check unserializable object! It's useless!
    - You may use `scanner`. It can check/substitute unsafe object.
      See `test/packss/core_test.clj` and `src/packss/core.clj`.
      **Unsafe objects are remained and never checked by default !!!**


## ChangeLog

- 1.0.1-SNAPSHOT
    - Fix `unpack` array of boolean, byte, short, char

- 1.0.0 (2014-03-25)
    - Fix `packable?` incorrectness
    - Rename `packable-classes` to `base-packable-classes`

- 0.3.0 (2014-03-24)
    - Fix over-sharing bug
        - See `shared-structure-determine-test`
          in `test/packss/core_test.clj`, this test didn't pass in past.

- 0.2.0 (2014-03-12)
    - Discriminate `list` and `vector` from `seq` strictly

- 0.1.0 (2014-03-08)
    - Initial release


## License

Copyright c 2014 ayamada

Distributed under the Eclipse Public License version 1.0


