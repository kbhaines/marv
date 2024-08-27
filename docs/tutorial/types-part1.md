# Types Tutorial - Part 1

## Introduction

Let's start from the bucket example in the last tutorial, and add a second bucket:

```
#lang marv

import types/gcp/storage as storage

module main(){

    my-project = env("MARV_GCP_PROJECT")

    bucket1 := storage:bucket {
        project = my-project,
        region = env("MARV_GCP_REGION"),
        name = strf("~a-hello-world1" my-project)
    }

    bucket2 := storage:bucket {
        project = my-project,
        region = env("MARV_GCP_REGION"),
        name = strf("~a-hello-world2" my-project)
    }
}
```

Clearly we are repeating ourselves: the `project` and `region` fields are replicated for each bucket. We can improve that by factoring the repeated attributes and then use the `<-` operator:
```
... 

defaults = {
    project = env("MARV_GCP_PROJECT"),
    region = env("MARV_GCP_REGION")
}

bucket1 := storage:bucket defaults <- {
    name = strf("~a-hello-world1" defaults.project)
}

bucket2 := storage:bucket defaults <- {
    name = strf("~a-hello-world2" defaults.project)
}
...
```

That's better. Now let's add some common labels to the buckets. There are a few different ways to do this, but they all have shortcomings. One idea is simply adding an attribute to the `defaults`:

```
defaults = {
    project = env("MARV_GCP_PROJECT"),
    region = env("MARV_GCP_REGION"),
    labels = { costcentre = "abc123", environment="dev" }
}
```

However, this wouldn't work if we wanted our buckets to have other labels as well:

```
bucket1 := storage:bucket defaults <- {
    name = strf("~a-hello-world1" defaults.project),
    labels = { purpose = "xyz" }
}
```

(the bucket would only have `purpose` as a label)

We could work around this by changing our bucket specification so that labels are combined with the `<-` operator:

```
bucket1 := storage:bucket defaults <- {
    name = strf("~a-hello-world1" defaults.project),
    labels = defaults.labels <- { purpose = "xyz" }
}
```

This works, but we're back to repeating ourselves, as well as introducing a cognitive burden - we need to remember to add those default labels for every bucket or resource that needs them.

## Types

So how might we do this using the `type` system in Marv?  Well, we can build a custom type for ourselves which already has the labels:

```
type labelledBucket = {
    identity(cfg) = cfg <- { 
        labels = (cfg.labels | {} ) <- { costcentre = "abc123", environment="dev" } 
    }
}
```

This isn't complete, we'll get to that in a moment. We do need to cover that `identity(cfg)` thing first, and types in general.  We'll take a little diversion at this point, so we can explain some foundational things about types.

## Question: What is a type?

**Answer: A type is a collection of functions.**

Basically, all resources in `marv` have a corresponding type defined for them - when you `import types/gcp/storage` you are importing the types exported by that module, and in our case we use `storage:bucket`.

When you declare a resource in marv, you are associating the type to the data inside the `{...}`. 

    bucket1 := storage:bucket { ... }

Marv's resource mangement system expects types to define certain functions so that it can manage the lifecycle of resources. One of these functions is `identity`. This function gets called very early on in the processing, and receives the resource's configuration (the config inside `{ ... }` ) in the `cfg` parameter. The function can update this - if it wants to. The result of the function is passed back to marv.

So looking back at our `labelledBucket` type, above, we can see that the `identity` function is setting a `labels` attribute in the `cfg` that it's given. But we still need to do more for this to work - as mentioned, marv is expecting our type to define certain other functions related to resource management. The original `storage:bucket` type already has these defined, so we can call those functions from our type:

```
type labelledBucket = {
    identity(cfg) = cfg <- { 
        labels = (cfg.labels | {} ) <- { costcentre = "abc123", environment="dev" } 
    }
    origin(cfg)         = storage:bucket.origin(cfg)
    create(cfg)         = storage:bucket.create(cfg)
    post-create(o, cfg) = storage:bucket.post-create(o, cfg)
    read(cfg)           = storage:bucket.read(cfg)
    post-read(o, cfg)   = storage:bucket.post-read(o, cfg)
    update(cfg)         = storage:bucket.update(cfg)
    post-update(o, cfg) = storage:bucket.post-update(o, cfg)
    delete(cfg)         = storage:bucket.delete(cfg)
}
```

(NB we're not going to cover all those functions in this tutorial; only `identity` is important for now)

That's a lot of boilerplate, and we don't like boilerplate. So `marv` has a shortcut that accomplishes the same thing:

```
type labelledBucket = {
    identity(cfg) = cfg <- { 
        labels = (cfg.labels | {} ) <- { costcentre = "abc123", environment="dev" } 
    }
    * = storage:bucket.*
}
```

`identity` is defined in `labelledBucket` and all other functions are found by looking at the functions defined by `storage:bucket`.

At this point our complete module looks like this:

```
#lang marv

import types/gcp/storage as storage

type labelledBucket = {
    identity(cfg) = cfg <- { 
        labels = (cfg.labels | {} ) <- { costcentre = "abc123", environment="dev" } 
    }
    * = storage:bucket.*
}

defaults = {
    project = env("MARV_GCP_PROJECT"),
    region = env("MARV_GCP_REGION") 
}

module main() {

    bucket1 := labelledBucket defaults <- {
        name = strf("~a-hello-world1" defaults.project)
    }

    bucket2 := labelledBucket defaults <- {
        name = strf("~a-hello-world2" defaults.project)
    }
    
}
```

That's the end of this part, but there's a lot more to Marv's type system. Carry on to [part 2](/docs/tutorial/types-part2.md) to learn more advanced things about types.
