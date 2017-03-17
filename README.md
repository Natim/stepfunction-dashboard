# stepfunction-dashboard

This is an Elm dashboard that you can sign-in and list the manual step
function that you need to signoff.

It relies on [Kinto](https://kinto.readthedocs.io/) and
[kinto-stepfunction](https://github.com/magopian/kinto-plugin-stepfunction)
plugin.

## How does it works?

- To log-in, enter your email address. You will be authenticated on
  Portier IDP.
- It will then list a list of manual stepfunction that you need to
  signoff or reject.
- You can use the UI to validate a manual step or reject it.

## How to run the dashboard locally

```sh
npm install
npm run live
```

## How to deploy the dashboard

```sh
npm run publish-to-gh-pages
```

You can then browse the admin here:
https://addons-shipping.github.io/stepfunction-dashboard
