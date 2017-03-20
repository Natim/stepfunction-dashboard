// Handle redirection with the bearer token in the hash
if (window.location.hash.indexOf("#auth=") === 0) {
  const token = window.location.hash.split('#auth=')[1];
  window.location.hash = "";
  localStorage.setItem("bearer", token);
}

// Retrieve saved state.
const bearer = localStorage.getItem("bearer");
const email = localStorage.getItem("email");
const {origin, pathname} = document.location;

app = Elm.Main.fullscreen({
  email: email,
  bearer: bearer,
  redirectUrl: `${origin}${pathname}#auth=`
});


// Handle storing some state information
app.ports.saveData.subscribe(function(data) {
  if (data.value === null) {
    localStorage.removeItem(data.key);
  } else {
    localStorage.setItem(data.key, data.value);
  }
});
