---
title: 'Curriculum Vitae'
---

### Maksymilian Owsianny

<p style="margin-bottom:0px"><Maksymilian.Owsianny@gmail.com></p>

<http://MaxOw.github.io>

#### Summary

I am a software engineer with interests in functional programming, software
correctness and computer graphics. I'm a big believer in strong type systems and
property testing of specifications - write it once and write it well. I have
experience leading a team in building a full-stack application, as well as
managing legacy systems.

#### Skills

  - **Functional Programming** - Declarative way is the right way. In places
    where correctness and longterm maintainability is required the best way
    about building such system is composing it from small, pure and well
    understood functions, and relaying on strong type system to guide you to
    success. I have a very good knowledge of Haskell programming language that
    shines in exactly such environment.

  - **Imperative Programming** - Turing machine is what it is and sometimes it
    is better to write code in a way that maps more naturally to the hardware.
    I have a good knowledge of imperative languages like c/c++ that can be used
    in performance critical applications. I also have a basic working knowledge
    of most of the other mainstream imperative languages and can be quickly
    brought to speed if it is required.

  - **Relaitonal Databases** - When dealing with large quantities of well
    structured data, relational databases are still an undisputed king. I have
    experience with both designing a database structure from scratch, optimizing
    SQL queries and general maintenance of old systems. I have worked with
    PostgreSQL, sqlite and others.

  - **System Administration** - I have experience with Unix-like systems, in
    particular deploying to Amazon AWS using NixOS/NixOps and generally
    administrating such a webserver. I also have experience in basic DevOps,
    that is scripting a build system for continuous integration, automatically
    performing testing, etc.

  - **Front End** - Front end is not something that I could say I enjoy however
    I do have experience designing and building front end of application for
    a client and if it's something that is critically needed I could handle it.
    I know reasonably well HTML/CSS and I've experimented with Elm and ghcjs.

#### Projects

  - **Computational Geometry** - My latest project, still early work in
    progress. Ultimately it will be a library containing useful data structures
    and algorithms from the field of computational geometry in the context of
    procedural graphics generation. Currently I have implemented algorithm
    dealing with set operations on polytopes that I've described in depth in
    a recent [blog post][blog-computational-geometry].

  - **CEF3-Bindings** - One thing that I believe is still missing in the Haskell
    ecosystem is a good story regarding GUI. On my way to scratch that
    particular itch I have decided to provide bindings to CEF3 and hopefully in
    the future, build something more nice and complete on top of that. For now
    using this bindings together with [threepenny-gui] is something that could
    be used instead, something that I have also described in a [blog
    post][blog-cef3].

  - **Marching Cubes** - It's an old project of mine but it has pretty
    screen-shots so I've decided to add it here. As the name suggest it is an
    implementation of the marching cubes algorithm, that is algorithm
    reconstructing a boundary polygons from a 3 dimensional density function. To
    checkout the code and pretty pictures follow
    [this github link][github-marching-cubes].

#### Work Experience

  - **Independent Consultant** (2012 - 2017) - The last few years I have spend
    as an independent consultant and as such had an opportunity to learn a lot
    and also experience working for different companies on exciting projects
    (like for example a robotic company, a bit of a highlight from the usual
    backend jobs). From this experience - besides some industry specific
    knowledge - I have learned interaction with clients and time management.

  - **Diagnostica** (2009 - 2011) - As a head of 3 person team, I was tasked
    with developing and maintaining a RIS (Radiology Information System) for
    a radiology clinic. It required designing database and building a front end
    application that helped medical personnel managing patient and imaging data,
    internal communication, and also interfaced with certain diagnostic
    instruments (Magnetic Resonance and Computer Tomograph).

#### Bonus: Happy Ending Problem

This is a little bonus for the mathematically inclined individuals that happened
to read this. Since I myself like to be exposed to a neat open problem once in
a while I decided that this time I'll do a bit of community service and bring up
one here for you. The simple version of this problem was initially proposed by
Esther Klein to Paul Erdős and George Szekeres as (paraphrasing): What is the
minimal number of randomly selected points on a plane, where no three points lay
on a line, that always have a subset of points forming a convex quadrilateral.
It is quite easy to provide a solution of this problem by geometric case
analysis, however, the generalization of that theorem to any convex polygon
resisted all attempts at solution for almost a century. I like it especially
because it seems like it should have a straightforward constructive solution yet
after a deeper delving it proves surprisingly resistant. Also the story behind
the name is quite nice: it was christened "Happy Ending Problem" by Erdős after
Klein and Szekeres ended up getting married.

[blog-computational-geometry]: https://maxow.github.io/posts/computational-geometry-set-operations-on-polytopes.html
[blog-cef3]: https://maxow.github.io/posts/creating-a-desktop-application-with-threepenny-gui-and-cef3.html
[threepenny-gui]: http://hackage.haskell.org/package/threepenny-gui
[github-marching-cubes]: https://github.com/MaxOw/MarchingCubes

