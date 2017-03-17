const KINTO_SERVER = "https://kinto.dev.mozaws.net/v1";  // No trailing slash
const KINTO_BUCKET = "stepfunction";
const KINTO_COLLECTION = "manual_steps";


// Handle redirection with the bearer token in the hash
if (window.location.hash.indexOf("#auth=") === 0) {
  const token = window.location.hash.split('#auth=')[1];
  window.location.hash = "";
  localStorage.setItem("bearer", token);
}

// Retrieve saved state.
const bearer = localStorage.getItem("bearer");
const email = localStorage.getItem("email");

app = Elm.Main.fullscreen({
  email: email,
  bearer: bearer,
  kintoServer: KINTO_SERVER,
  kintoBucket: KINTO_BUCKET,
  kintoCollection: KINTO_COLLECTION
});

// Handle calling to Kinto-Portier login resource.
app.ports.authenticate.subscribe(function(emailValue) {
  const {origin, pathname} = document.location;
  const form = document.createElement("form");
  const email = document.createElement("input");
  const redirect = document.createElement("input");

  form.method = "POST";
  // XXX: Fixme handle trailing slash in KINTO_SERVER
  form.action = `${KINTO_SERVER}/portier/login`;
  form.style.display = "none";

  email.name = "email";
  email.value = emailValue;
  form.appendChild(email);

  redirect.value = `${origin}${pathname}#auth=`;
  redirect.name = "redirect";
  form.appendChild(redirect);

  document.body.appendChild(form);
  form.submit();
});


// Handle storing some state information
app.ports.saveData.subscribe(function(data) {
  if (data.value === null) {
    localStorage.removeItem(data.key);
  } else {
    localStorage.setItem(data.key, data.value);
  }
});
