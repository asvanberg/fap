# Fleet action participation

A simple service used to track participation in fleets in EVE Online.
Intended to be used as a backend service by other systems that interface
with users and handle authentication processes.

## Roadmap
* Alliance fleets

There is currently no (realistic) way of fetching which alliance a character
belongs to. The `/corporations/:corporationID/` endpoint is not enabled "yet" and
traversing the entire `/alliances/` endpoint to find the characters corporation
will never work.

Either we have to use another API that provide this functionality or we wait for
CCP to implement the `/corporations/:corporationID/` endpoint.