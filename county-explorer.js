(() => {
  const dataNode = document.getElementById("county-results-data");
  const search = document.getElementById("county-search");
  const options = document.getElementById("county-options");
  const status = document.getElementById("county-search-status");
  const results = document.getElementById("county-results");

  if (!dataNode || !search || !options || !status || !results) {
    console.error("County explorer markup is incomplete.");
    return;
  }

  let rows;
  try {
    rows = JSON.parse(dataNode.textContent);
  } catch (error) {
    status.textContent = "County results could not be loaded.";
    console.error("Could not parse embedded county results.", error);
    return;
  }

  if (!Array.isArray(rows) || rows.length === 0) {
    status.textContent = "No county result rows are available.";
    return;
  }

  const counties = [...new Set(rows.map((row) => row.county))].sort((a, b) =>
    a.localeCompare(b)
  );
  const countyRows = new Map(
    counties.map((county) => [
      county.toLocaleLowerCase(),
      rows.filter((row) => row.county === county),
    ])
  );

  counties.forEach((county) => {
    const option = document.createElement("option");
    option.value = county;
    options.append(option);
  });

  const formatVotes = (votes) => new Intl.NumberFormat("en-US").format(votes);
  const makeElement = (tag, className, text) => {
    const element = document.createElement(tag);
    if (className) element.className = className;
    if (text !== undefined) element.textContent = text;
    return element;
  };

  const makeStat = (label, value, note) => {
    const card = makeElement("div", "stat-card");
    card.append(makeElement("span", "stat-label", label));
    card.append(makeElement("strong", "stat-value", value));
    card.append(makeElement("span", "stat-footnote", note));
    return card;
  };

  const renderCounty = (typedName) => {
    const countyName = counties.find(
      (county) => county.toLocaleLowerCase() === typedName.trim().toLocaleLowerCase()
    );

    if (!countyName) {
      results.replaceChildren();
      status.textContent = typedName.trim()
        ? "Choose a county from the suggestions to view its results."
        : "Search or select a county to view its results.";
      return;
    }

    const countyResults = countyRows
      .get(countyName.toLocaleLowerCase())
      .slice()
      .sort((a, b) => b.votes - a.votes);
    if (countyResults.some((row) => !Number.isFinite(Number(row.votes)))) {
      results.replaceChildren();
      status.textContent = "This county has invalid vote totals in the embedded data.";
      return;
    }

    const totalVotes = countyResults.reduce((sum, row) => sum + Number(row.votes), 0);
    const leader = countyResults[0];
    const runnerUp = countyResults[1];
    const tied = runnerUp && Number(leader.votes) === Number(runnerUp.votes);
    const marginVotes = runnerUp ? Number(leader.votes) - Number(runnerUp.votes) : null;
    const marginPoints = runnerUp
      ? Number(leader.vote_percentage) - Number(runnerUp.vote_percentage)
      : null;
    const reportingDate = new Date(leader.reporting_timestamp);
    const reportingTime = Number.isNaN(reportingDate.getTime())
      ? leader.reporting_timestamp
      : new Intl.DateTimeFormat("en-US", {
          timeZone: "America/Los_Angeles",
          dateStyle: "medium",
          timeStyle: "short",
        }).format(reportingDate) + " PT";

    const heading = makeElement("div", "county-results-heading");
    heading.append(makeElement("h2", "", `${countyName} County`));
    heading.append(
      makeElement("span", "county-fips", `County FIPS ${leader.FIPS}`)
    );

    const cards = makeElement("div", "stat-grid county-explorer-stats");
    cards.append(
      makeStat("Governor votes", formatVotes(totalVotes), "Reported candidate votes")
    );
    cards.append(
      makeStat(
        tied ? "County lead" : "County leader",
        tied ? "Tie" : leader.candidate,
        tied ? "No resolved lead" : `${leader.party} · ${Number(leader.vote_percentage).toFixed(1)}%`
      )
    );
    cards.append(
      makeStat(
        "Victory margin",
        tied || marginPoints === null ? "Unresolved" : `${marginPoints.toFixed(2)} pp`,
        tied || marginVotes === null
          ? "Candidates are tied"
          : `${formatVotes(marginVotes)} votes`
      )
    );

    const comparison = makeElement("section", "county-candidate-comparison");
    comparison.append(makeElement("h3", "", "Candidate comparison"));
    countyResults.forEach((row) => {
      const percent = Number(row.vote_percentage);
      const candidateRow = makeElement("div", "county-candidate-row");
      candidateRow.append(
        makeElement("span", "", `${row.candidate} · ${row.party}`)
      );
      const track = makeElement("div", "candidate-bar-track");
      track.setAttribute(
        "role",
        "img"
      );
      track.setAttribute(
        "aria-label",
        `${row.candidate}: ${percent.toFixed(2)} percent`
      );
      const fill = makeElement(
        "div",
        `candidate-bar-fill ${
          row.party === "Democratic"
            ? "democratic"
            : row.party === "Republican"
              ? "republican"
              : "other"
        }`
      );
      fill.style.width = `${Math.max(0, Math.min(100, percent))}%`;
      track.append(fill);
      candidateRow.append(track);
      candidateRow.append(
        makeElement("strong", "", `${percent.toFixed(2)}% · ${formatVotes(row.votes)}`)
      );
      comparison.append(candidateRow);
    });

    const report = makeElement(
      "p",
      "small-note",
      `County API reporting timestamp: ${reportingTime}. Results are unofficial.`
    );
    results.replaceChildren(heading, cards, comparison, report);
    status.textContent = `Showing ${countyName} County.`;
  };

  search.addEventListener("input", () => renderCounty(search.value));
  search.value = counties.find((county) => county === "Los Angeles") || counties[0];
  renderCounty(search.value);
})();
