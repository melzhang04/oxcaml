import { test, expect, Page } from "@playwright/test";

const BASE_URL = "http://localhost:8000/";

test.beforeEach(async ({ page }) => {
  await page.goto(BASE_URL);
});

test.describe("Setup Screen", () => {
  test("shows Battleship title and mode buttons", async ({ page }) => {
    await expect(page.getByRole("heading", { name: "BATTLESHIP" })).toBeVisible();
    await expect(page.getByText("Select Game Mode")).toBeVisible();

    await expect(page.getByRole("button", { name: "Player vs Player" }))
      .toBeVisible();
    await expect(page.getByRole("button", { name: "Vs AI (Easy)" }))
      .toBeVisible();
    await expect(page.getByRole("button", { name: "Vs AI (Hard)" }))
      .toBeVisible();
  });

  test("starts PvP and loads placement screen", async ({ page }) => {
    await page.getByRole("button", { name: "Player vs Player" }).click();

    await expect(page.getByText("Place Your Ships")).toBeVisible();
    await expect(page.getByRole("button", { name: /Rotate:/ })).toBeVisible();
  });
});


async function startPvP(page: Page) {
  await page.getByRole("button", { name: "Player vs Player" }).click();
  await expect(page.getByText("Place Your Ships")).toBeVisible();
}

async function finishPlacement(page: Page) {
  // Player 1
  await page.getByRole("button", { name: "Randomize Fleet" }).click();
  await expect(page.getByRole("button", { name: "Confirm Placement" }))
    .toBeEnabled();
  await page.getByRole("button", { name: "Confirm Placement" }).click();

  // Player 2
  await expect(page.getByText("PLAYER 2")).toBeVisible();
  await page.getByRole("button", { name: "Randomize Fleet" }).click();
  await expect(page.getByRole("button", { name: "Confirm Placement" }))
    .toBeEnabled();
  await page.getByRole("button", { name: "Confirm Placement" }).click();

  // Game screen
  await expect(page.getByText(/TURN/)).toBeVisible();
}

test.describe("Placement Screen", () => {
  test.beforeEach(async ({ page }) => {
    await startPvP(page);
  });

  test("rotate button toggles correctly", async ({ page }) => {
    const rotate = page.getByRole("button", { name: /Rotate:/ });

    await rotate.click();
    await expect(rotate).toHaveText(/Vertical/);

    await rotate.click();
    await expect(rotate).toHaveText(/Horizontal/);
  });

  test("randomize fleet populates ships", async ({ page }) => {
    await page.getByRole("button", { name: "Randomize Fleet" }).click();

    await expect(page.locator(".ship")).toHaveCount(5);
  });

  test("confirm button disabled until ships placed", async ({ page }) => {
    const confirm = page.getByRole("button", { name: "Confirm Placement" });
    await expect(confirm).toBeDisabled();

    await page.getByRole("button", { name: "Randomize Fleet" }).click();
    await expect(confirm).toBeEnabled();
  });
});

test.describe("Gameplay Screen", () => {
  test.beforeEach(async ({ page }) => {
    await startPvP(page);
    await finishPlacement(page);
  });

  test("renders both boards", async ({ page }) => {
    await expect(page.getByText(/DEFENSE/)).toBeVisible();
    await expect(page.getByText(/OFFENSE/)).toBeVisible();
  });

  test("clicking offense board creates hit/miss marker", async ({ page }) => {
    const offense = page.locator(".board-container").nth(1);

    await offense.locator(".cell").first().click();

    await expect(page.locator(".marker")).toHaveCount(1);
  });

  test("turn indicator updates after shot", async ({ page }) => {
    const offense = page.locator(".board-container").nth(1);
    const indicator = page.locator(".turn-indicator");

    const before = await indicator.textContent();

    await offense.locator(".cell").first().click();

    await expect(indicator).not.toHaveText(before!);
  });
});

test("reset returns to title screen", async ({ page }) => {
  await startPvP(page);
  await finishPlacement(page);

  await page.getByRole("button", { name: "Reset Game" }).click();
  await expect(page.getByRole("heading", { name: "BATTLESHIP" })).toBeVisible();
});
