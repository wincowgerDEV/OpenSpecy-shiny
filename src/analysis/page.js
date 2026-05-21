import { getOpenSpecyEngine, supportedOpenSpecyLibraries } from "./openSpecyApi.js";

const form = document.querySelector("[data-analysis-form]");
const fileInput = document.querySelector("[data-file-input]");
const libraryInput = document.querySelector("[data-library-input]");
const analyzeButton = document.querySelector("[data-analyze-button]");
const statusRegion = document.querySelector("[data-status]");
const errorRegion = document.querySelector("[data-error]");
const resultsRegion = document.querySelector("[data-results]");
const emptyState = document.querySelector("[data-empty-state]");
const resultsSummary = document.querySelector("[data-results-summary]");
const resultsTable = document.querySelector("[data-results-table]");

let slowTimer = null;

populateLibraries();
showEnvironmentProblemIfNeeded();

form.addEventListener("submit", async (event) => {
  event.preventDefault();
  clearOutput();

  const environmentProblem = getEnvironmentProblem();
  if (environmentProblem) {
    showError(environmentProblem);
    setStatus("Open the built static site over HTTP before starting analysis.");
    return;
  }

  const file = fileInput.files && fileInput.files[0];
  if (!file) {
    showError("Choose a spectrum file before starting analysis.");
    return;
  }

  const options = readOptions();
  setBusy(true);
  setStatus("Preparing the Open Specy analysis engine...");
  slowTimer = window.setTimeout(() => {
    setStatus("This can take a little while the first time because R is loading in your browser.");
  }, 7000);

  try {
    const engine = await getOpenSpecyEngine({ onStatus: setStatus });
    const result = await engine.analyzeFile(file, options, setStatus);
    renderResults(result);
    setStatus("Analysis complete.");
  } catch (error) {
    showError(error && error.message ? error.message : String(error));
    setStatus("Analysis did not complete.");
  } finally {
    window.clearTimeout(slowTimer);
    slowTimer = null;
    setBusy(false);
  }
});

function populateLibraries() {
  const supported = supportedOpenSpecyLibraries();
  libraryInput.textContent = "";

  for (const library of supported) {
    const option = document.createElement("option");
    option.value = library;
    option.textContent = library === "medoid" ? "Medoid" : "Model";
    libraryInput.append(option);
  }
}

function showEnvironmentProblemIfNeeded() {
  const problem = getEnvironmentProblem();
  if (!problem) {
    return;
  }

  analyzeButton.disabled = true;
  showError(problem);
  setStatus("Open the built static site over HTTP before starting analysis.");
}

function getEnvironmentProblem() {
  if (window.location.protocol === "file:") {
    return [
      "This analysis page cannot run from a file:// URL.",
      "Build the site, serve the site/ directory over HTTP, and open /analysis/."
    ].join(" ");
  }

  if (/\/src\/analysis\/?/.test(window.location.pathname.replace(/\\/g, "/"))) {
    return [
      "You opened the source analysis page.",
      "Run the static build and open the generated site/analysis/ page so webR, R wrappers, and medoid/model assets resolve correctly."
    ].join(" ");
  }

  return "";
}

function readOptions() {
  const formData = new FormData(form);
  return {
    library: String(formData.get("library") || "medoid"),
    smooth: formData.get("smooth") === "on",
    baseline: formData.get("baseline") === "on",
    match_method: String(formData.get("match_method") || "correlation"),
    spectrum_type: String(formData.get("spectrum_type") || "both"),
    top_n: Number(formData.get("top_n") || 5),
    min_match: Number(formData.get("min_match") || 0)
  };
}

function setBusy(isBusy) {
  analyzeButton.disabled = isBusy;
  analyzeButton.textContent = isBusy ? "Analyzing..." : "Analyze";
}

function setStatus(message) {
  statusRegion.textContent = message;
}

function showError(message) {
  errorRegion.hidden = false;
  errorRegion.textContent = message;
}

function clearOutput() {
  errorRegion.hidden = true;
  errorRegion.textContent = "";
  resultsRegion.hidden = true;
  emptyState.hidden = false;
  resultsSummary.textContent = "";
  resultsTable.replaceChildren();
}

function renderResults(result) {
  const rows = Array.isArray(result.matches) ? result.matches : [];
  resultsRegion.hidden = false;
  emptyState.hidden = true;

  const summary = result.summary || {};
  resultsSummary.textContent = [
    `${rows.length} match rows`,
    summary.n_spectra ? `${summary.n_spectra} spectra` : null,
    summary.n_wavenumber ? `${summary.n_wavenumber} wavenumbers` : null,
    result.library ? `${result.library} library` : null
  ].filter(Boolean).join(" | ");

  if (!rows.length) {
    const empty = document.createElement("p");
    empty.className = "empty-results";
    empty.textContent = "No matches were returned for the selected file and options.";
    resultsTable.replaceChildren(empty);
    return;
  }

  const columns = [
    "object_id",
    "rank",
    "material_class",
    "spectrum_identity",
    "sample_name",
    "organization",
    "spectrum_type",
    "match_val",
    "passed_threshold"
  ].filter((column) => rows.some((row) => row[column] !== undefined && row[column] !== null));

  const table = document.createElement("table");
  const thead = document.createElement("thead");
  const tbody = document.createElement("tbody");
  const headerRow = document.createElement("tr");

  for (const column of columns) {
    const th = document.createElement("th");
    th.scope = "col";
    th.textContent = humanizeColumn(column);
    headerRow.append(th);
  }

  thead.append(headerRow);

  for (const row of rows) {
    const tr = document.createElement("tr");
    for (const column of columns) {
      const td = document.createElement("td");
      td.textContent = formatValue(row[column]);
      tr.append(td);
    }
    tbody.append(tr);
  }

  table.append(thead, tbody);
  resultsTable.replaceChildren(table);
}

function humanizeColumn(column) {
  return column
    .replace(/_/g, " ")
    .replace(/\b\w/g, (letter) => letter.toUpperCase());
}

function formatValue(value) {
  if (value === null || value === undefined) {
    return "";
  }
  if (typeof value === "number") {
    return Number.isFinite(value) ? value.toPrecision(4) : "";
  }
  if (typeof value === "boolean") {
    return value ? "Yes" : "No";
  }
  return String(value);
}
