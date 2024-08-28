# A First Example - hello-bucket

## Before you start

> Make sure you have [installed and configured](../installation.md) `marv`.
> Also ensure that you have created an `alias` to invoke `marv`:
>     
>     alias marv="racket command.rkt"
> **You will need a GCP project that you can create your resources in.**

## Let's start!

We are going to use "The Ubiquitous Bucket Example" - a recipe that will create
a storage bucket in a GCP project. After we've deployed it, we'll walk through
the code.

## The Code
Start by copy/pasting this code into a new file `hello-world.mrv`:

```
#lang marv

import types/gcp/storage as storage

module main() {

    my-project = env("MARV_GCP_PROJECT")

    hello-bucket := storage:bucket {
        project = my-project,
        region = env("MARV_GCP_REGION"),
        name = strf("~a-hello-world" my-project)
    }
}
```

## Deploying to GCP
 Run the following commands:

    export MARV_GCP_PROJECT=<your-gcp-project>
    export MARV_GCP_REGION=europe-west1  # change to suit your location
    export GCP_ACCESS_TOKEN=`gcloud auth print-access-token`
    
    marv --plan hello-world.mrv

Marv should report that it will create the bucket and you can go ahead and deploy it like this:

    marv --apply hello-world.mrv

Your new bucket is now ready for action! You will also notice that marv has
created a **state file** in your current directory - this file is important as
it holds details on the resources that have been created, and what attributes
were configured.

## Break it down...

The first line of a marv file  tells the Racket interpreter to invoke Marv's DSL:

    #lang marv

Next we need to import the types for the GCP storage API - the types have to be
imported (or declared) before they can be used by a module. Importing a module
`as <alias>` will require all of its resources to be prefixed with `<alias>:`

    import types/gcp/storage as storage


> ### Note
> The type system is a part of what makes marv a bit different from other IAC
tools; this will be covered in a separate tutorial.

All resources must be declared inside a module, and there must be a `main`
module declared in the top-level file passed on the command line:

    module main() {...}

First up, we declare a variable that gets its value from the `MARV_GCP_PROJECT`
environment variable:

    my-project = env("MARV_GCP_PROJECT")

Now we come to our bucket’s resource declaration: `hello-bucket` is a `bucket`
resource (from the `storage` module, imported earlier):

    hello-bucket := storage:bucket ...

> ## Note
> A resource declaration is generally of the form:
> 
>     name := type { config... }
>   
> `type` is the kind of resource supported by the cloud provider, such as
`instance` or `secret` (NB this isn't *strictly* true, but it suffices for now!)

Finally, we have the body of our resource declaration:

```
{
    project = my-project,
    region = env("MARV_GCP_REGION"),
    name = strf("~a-hello-world" my-project)
}
```

This is a `config-object` which is a comma-separated list of `name=value` attributes, enclosed in `{  }` (actually, this is a *map* in Marv syntax)

`name` is assigned the results of the `strf` function which replaces `~a` by the
value of the `project`, which is from the `MARV_GCP_PROJECT` environment
variable.

> ## Note 
> We are working on auto-discovering which fields are immutable so
you won't need to do as much of this manual tagging in a future
release.

# Next steps

That completes our first steps with `marv` - in the next section we'll cover
more of the language features. However, if you want to stop and delete all the
resources for now you can run the purge command:

    marv --purge --apply hello-world.mrv

# MORE TUTORIALS COMING SOON

In the meantime, you can look at the
[examples](https://github.com/marvlogic/marv/tree/main/examples/gcp) for more
information.