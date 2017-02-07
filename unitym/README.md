One of the nice things about WAI is that it allows us to combine multiple
different server frameworks in a single web server- allowing us to use
(for example) Yesod to develop the server-side rendered HTML part of the
website, and Servant to develop the REST API part.  But this gives rise
to a desire to share code between the different frameworks.  This is
where UnityM comes in.


