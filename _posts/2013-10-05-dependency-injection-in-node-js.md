---
title: Dependency Injection in node.js
date: 2013-10-05T19:26:44Z
---
node.js modules are singletons. If you `require('foo')` from one module, and
then `require('foo')` from a different module, you'll get the same instance.

Because of this, it's a convenient shortcut to define shared variables at
module scope. They're not global, because they're per-module, but they have all
of the downsides of static variables in C++/C#/Java-style languages, because
they're singletons.

This can make dependency injection trickier, whether you're using class-style node.js, or factory method-style node.js (I just made those terms up). Consider:

    // In module A:
    var x = require('x');

    // In module B:
    var x = require('x');

    // In module X:
    var db = require('db');
    var user_cache = db.query('SELECT * FROM Users;');
    module.exports = {
        get_users: function() {
            // ...
        }
    };
    
Whoops, now A and B have an implicit dependency on the DB. That's gonna make
unit testing harder.

So, maybe you decide to write the module like this:

    // In module A:
    var db = require('db');
    var x = require('x')(db);

    // In module B:
    var db = require('db');
    var x = require('x')(db);

So far, so good. We can stub the database dependency:

    // In tests/X
    var stub_db = {
        query: function() {
            // return some canned data.
        };
    };
    var x = require('x')(stub_db);

But we end up in trouble when we attempt to implement the new module:

    // In module X - attempt 1:
    // Can't do this, because 'db' isn't available.
    var user_cache = db.query('...');
    module.exports = function(db) {
        return {
            get_users: function() {
                // ...
            }
        };
    };

    // In module X - attempt 2:
    module.exports = function(db) {
        // Can't do this, because the cache will be local to this closure.
        var user_cache = db.query('...');
        return { get_users: ... };
    };

    // In module X - attempt 3:
    var user_cache = null;
    module.exports = function(db) {
        // Can't do this, because the cache will get rebuilt each time the
        // exported factory function is called. Which will be at startup, which
        // will murder performance.
        user_cache = db.query('...');
        return { get_users: /* ... */ };
    };

    // In module X - attempt 4:
    var user_cache = null;
    module.exports = function(db) {
        // Better, but can we improve it?
        if (!user_cache) {
            user_cache = db.query('...');
        }

        return { get_users: /* ... */ };
    };

Of these, attempt 4 is probably the best, but -- depending on your opinion --
there's a problem with that one as well: all of the places that `require('x')`
need to know about the database dependency.

Maybe we can do something like this:

    // In app.js:
    var db = require('db');
    var x = require('x');
    x.use(db);

    // In module A:
    var x = require('x');

    // In module X - attempt 5:
    var _db = null;         // If you need to.
    var user_cache = null;
    module.exports = {
        use: function(db) {
            _db = db;   // If you need to.
            user_cache = db.query('...');
        }
        get_users: function() { /* ... */ }
    };

There are, in my opinion, two problems with this:

 1. We've now hidden X's dependency on db. I'm not sure I'm totally happy with
    that, but it's what a DI container would have done for us: A depends on X,
    which depends on DB, but that's the container's problem, not A's.
 2. I'm not sure that it makes unit testing easier, because we've still got a
    bunch of singleton shared state. Maybe that's a different problem, though.
