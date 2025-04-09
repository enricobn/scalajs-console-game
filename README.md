DEVELOP
=======
you can run `sbt ~fastOptJS` to fast compile and recompile on changes
then open index_develop.html

TEST
=====
you need node in the path:  
run `sbt test`

PRODUCTION
==========
to build an optimized javascript to use in production run 'sbt fullOptJS'
then open index.html

NOTES
======
- the content of a file could be AnyRef, but it's better if it's an immutable data structure,
  so to change it, you must call setContent which checks for user permissions 