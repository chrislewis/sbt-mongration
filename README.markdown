# sbt-mongration

sbt-mongration is a plugin for sbt [sbt](http://code.google.com/p/simple-build-tool/)
to speed development against [mongodb](http://www.mongodb.org/).

    > mongo-reset
    
## Seeding the database

One of the main functions of the plugin is to make bootstrapping the database simple.
This is handled by the seed file, a JSON document that contains a list of collections,
and a little bit of metadata, which is used to create a fresh database.

As a simple example, consider the following seed, which would create a single
collection named "posts" with a single document:

    [
      {
        "name": "posts",
        "docs": [
          {
            "title": "hello world!",
            "body": "hi",
            "tags": [ "mongo", "scala" ]
          }
        ]
      }
    ]
    
The structure should be fairly obvious. Every object under "docs" is a document
that will be persisted to the containing collection, verbatim*. The other
elements comprise the metadata used to create the collections and the database
as a whole.