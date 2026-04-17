# Provide a connection to the relevant Oracle database

Provide a connection to the relevant Oracle database

## Usage

``` r
db_connection(schema = "PRU_READ")
```

## Arguments

- schema:

  the Oracle DB schema to connect to.

## Value

A connection to a database (specifically an implementation of
[DBI::DBIConnection](https://dbi.r-dbi.org/reference/DBIConnection-class.html)
for an Oracle database.)

## Note

The `schema` is in fact the prefix of the environment variables where
the credentials are stored, like `<schema>_USR`, `<schema>_PWD` and
`<schema>_DBNAME`. Possible values for `schema` are `PRU_READ`,
`PRU_PROD`, `PRU_DEV`, `PRU_TEST`, ...

## Examples

``` r
if (FALSE) { # \dontrun{
# Within a function call (or interactive session, a restart will close the
# `withr` calls) you should use the following snippet of code
withr::local_envvar(c(TZ = "UTC", ORA_SDTZ = "UTC", NLS_LANG = ".AL32UTF8"))
# connect with PRU_READ credentials
conn <- db_connection("PRU_READ") |> withr::local_db_connection()
# or alternativelly more basically (you need to explicitly close the
# the DB connection):
conn <- db_connection()
# ... perform other API operations re-using the same connection
# ...
DBI::dbDisconnect(conn)
} # }
```
