# Catalog service

This is an example catalog service. It will provide a RESTful API to catalog
https://schema.org/CreativeWork objects. It is very basic but requires enough
functionality to resemble a modern service.

## Domain Model

### CreativeWork

The data model will simply be a https://schema.org/CreativeWork with the following fields:

- https://schema.org/name: string
- https://schema.org/description: string
- https://schema.org/author: [URL] to a /people/{id}; the people must exist before the work can be stored

This API will be as follows:

- GET /works/
- POST /works/{id}
- DELETE /works/{id}
- PUT /works/{id}
- GET /works/{id}

### People

The data model of people is a https://schema.org/Person with the following fields:

- https://schema.org/name: string

This API will be as follows: 

- GET /people/
- POST /people/
- DELETE /people/{id}
- PUT /people/{id}
- GET /people/{id}

## Project layout

- Entity/CreativeWork - Provides the domain types for the works
- Entity/Author - Provides the domain types for the authors
- Repository/CreativeWork - Provides the repository interface for works
- Repository/CreativeWork/Postgres - Provides the repository implementation for Postgres
- Repository/CreativeWork/Mock - Provides the mock repository implementation for testing
- Repository/Person - Provides the repository interface for authors
- Repository/Person/Postres - Provides the repository implementation for Postgres
- Repository/Person/Mock - Provides the mock repository implementation for testing
- Transport/HTTP - Defines the RESTful HTTP interface to the CreativeWork and Author services

## Testing

In order to test this service I will have to figure out how to swap the repositories with mocks when testing.

Resources:

- https://felixmulder.com/writing/2019/10/05/Designing-testable-components.html

![I have no idea](./ihave.jpg)
