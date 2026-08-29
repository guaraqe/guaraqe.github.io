window.addEventListener("TrunkApplicationStarted", async () => {
  const loading = document.getElementById("loading");
  if (loading) {
    loading.style.display = "none";
  }

  const bindings = window.wasmBindings;
  if (bindings?.start) {
    await bindings.start();
  } else {
    console.error("start() function not found in WASM bindings");
  }
});
