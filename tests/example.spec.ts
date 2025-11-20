import { test, expect, Page } from "@playwright/test";

const BASE_URL = "https://legendary-fishstick-jpgpgqqgxw62pvgj-8000.app.github.dev/";

async function openGame(page: Page) {
  await page.goto(BASE_URL, { waitUntil: "networkidle" });
  await expect(page.getByRole("heading", { name: "BATTLESHIP" })).toBeVisible({
    timeout: 10000,
  });
}

async function startPvP(page: Page) {
  await openGame(page);
  await page.getByRole("button", { name: "Player vs Player" }).click();
  await expect(page.getByText("PLAYER 1 — Place Your Ships")).toBeVisible({
    timeout: 10000,
  });
}

async function finishPlacement(page: Page) {
  await page.getByRole("button", { name: "Randomize Fleet" }).click();
  await expect(
    page.getByRole("button", { name: "Confirm Placement" })
  ).toBeEnabled({ timeout: 5000 });
  await page.getByRole("button", { name: "Confirm Placement" }).click();

  await expect(
    page.getByText("PLAYER 2 — Place Your Ships")
  ).toBeVisible({ timeout: 10000 });
  await page.getByRole("button", { name: "Randomize Fleet" }).click();
  await expect(
    page.getByRole("button", { name: "Confirm Placement" })
  ).toBeEnabled({ timeout: 5000 });
  await page.getByRole("button", { name: "Confirm Placement" }).click();

  await expect(page.getByText("PLAYER 1’S TURN")).toBeVisible({ timeout: 10000 });
}

test.describe("Setup Screen", () => {
  test("shows Battleship title and mode buttons", async ({ page }) => {
    await openGame(page);
    await expect(page.getByText("Select Game Mode")).toBeVisible();
    await expect(
      page.getByRole("button", { name: "Player vs Player" })
    ).toBeVisible();
    await expect(
      page.getByRole("button", { name: "Vs AI (Easy)" })
    ).toBeVisible();
    await expect(
      page.getByRole("button", { name: "Vs AI (Hard)" })
    ).toBeVisible();
  });

  test("enters placement screen after selecting Player vs Player", async ({
    page,
  }) => {
    await startPvP(page);
    await expect(page.getByText("Rotate: Horizontal")).toBeVisible();
  });
});

test.describe("Placement Screen", () => {
  test.beforeEach(async ({ page }) => {
    await startPvP(page);
  });

  test("rotate button toggles orientation", async ({ page }) => {
    const rotate = page.getByRole("button", { name: /Rotate:/ });
    await rotate.click();
    await expect(rotate).toHaveText("Rotate: Vertical");
    await rotate.click();
    await expect(rotate).toHaveText("Rotate: Horizontal");
  });

  test("randomize fleet populates ships", async ({ page }) => {
    const randomize = page.getByRole("button", { name: "Randomize Fleet" });
    await randomize.click();
    await page.waitForSelector(".ship", { timeout: 5000 });
    const ships = await page.locator(".ship").count();
    expect(ships).toBeGreaterThan(0);
  });

  test("confirm button disables until all ships placed", async ({ page }) => {
    const confirm = page.getByRole("button", { name: "Confirm Placement" });
    await expect(confirm).toHaveAttribute("disabled", "disabled");
    await page.getByRole("button", { name: "Randomize Fleet" }).click();
    await expect(confirm).toBeEnabled({ timeout: 5000 });
  });
});

test.describe("Gameplay Screen", () => {
  test.beforeEach(async ({ page }) => {
    await startPvP(page);
    await finishPlacement(page);
  });

  test("board renders correctly for both players", async ({ page }) => {
    await expect(
      page.getByText("PLAYER 1’S BOARD (DEFENSE)")
    ).toBeVisible({ timeout: 10000 });
    await expect(
      page.getByText("PLAYER 2’S BOARD (OFFENSE)")
    ).toBeVisible({ timeout: 10000 });
  });

  test("clicking on offense board shows hit/miss marker", async ({ page }) => {
    await page.waitForSelector(".cell", { timeout: 5000 });
    const cells = await page.$$(".cell");
    await cells[0].click();
    await page.waitForSelector(".marker", { timeout: 5000 });
    await expect(page.locator(".marker")).toBeVisible();
  });

  test("turn indicator updates after a move", async ({ page }) => {
    await page.waitForSelector(".cell", { timeout: 5000 });
    const cells = await page.$$(".cell");
    await cells[0].click();
    await expect(
      page.getByText("PLAYER 2’S TURN")
    ).toBeVisible({ timeout: 10000 });
  });
});

test.describe("Reset Functionality", () => {
  test("reset button returns to title screen", async ({ page }) => {
    await startPvP(page);
    await finishPlacement(page);
    await page.waitForSelector(".reset-button", { timeout: 5000 });
    await page.getByRole("button", { name: "Reset Game" }).click();
    await expect(
      page.getByRole("heading", { name: "BATTLESHIP" })
    ).toBeVisible({ timeout: 5000 });
  });
});
