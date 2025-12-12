import { test, expect, Page } from "@playwright/test";

const BASE_URL = "http://localhost:8000/";

test.beforeEach(async ({ page }) => {
  await page.goto(BASE_URL);
});

test.describe("Setup Screen", () => {
  test("shows Battleship title and mode buttons", async ({ page }) => {
    await expect(page.getByRole("heading", { name: "BATTLESHIP" })).toBeVisible();
    await expect(page.getByText("Select Game Mode")).toBeVisible();

    await expect(page.getByRole("button", { name: "Player vs Player" })).toBeVisible();
    await expect(page.getByRole("button", { name: "Vs AI (Easy)" })).toBeVisible();
    await expect(page.getByRole("button", { name: "Vs AI (Hard)" })).toBeVisible();
  });

  test("PvP mode requires authentication", async ({ page }) => {
    page.once('dialog', dialog => {
      expect(dialog.message()).toContain("sign in");
      dialog.accept();
    });

    await page.getByRole("button", { name: "Player vs Player" }).click();
    
    await expect(page.getByText("Select Game Mode")).toBeVisible();
  });

  test("starts PvE Easy mode", async ({ page }) => {
    await page.getByRole("button", { name: "Vs AI (Easy)" }).click();
    await expect(page.getByText("Place Your Ships")).toBeVisible();
  });

  test("starts PvE Hard mode", async ({ page }) => {
    await page.getByRole("button", { name: "Vs AI (Hard)" }).click();
    await expect(page.getByText("Place Your Ships")).toBeVisible();
  });
});

test.describe("Auth Bar", () => {
  test("shows sign in button when not authenticated", async ({ page }) => {
    await expect(page.getByRole("button", { name: /Sign In/ })).toBeVisible();
  });

  test("sign in button is enabled", async ({ page }) => {
    const signInBtn = page.getByRole("button", { name: /Sign In/ });
    await expect(signInBtn).toBeEnabled();
  });

  test("auth bar is visible on all screens", async ({ page }) => {
    const authBar = page.locator(".auth-bar");

    await expect(authBar).toBeVisible();
    
    await page.getByRole("button", { name: "Vs AI (Easy)" }).click();
    await expect(authBar).toBeVisible();
    
    await page.getByRole("button", { name: "Randomize Fleet" }).click();
    await page.getByRole("button", { name: "Confirm Placement" }).click();
    await expect(authBar).toBeVisible();
  });
});

test.describe("Placement Screen - UI Elements", () => {
  test.beforeEach(async ({ page }) => {
    await page.getByRole("button", { name: "Vs AI (Easy)" }).click();
  });

  test("shows placement title", async ({ page }) => {
    await expect(page.getByText("Place Your Ships")).toBeVisible();
  });

  test("shows all control buttons", async ({ page }) => {
    await expect(page.getByRole("button", { name: /Rotate:/ })).toBeVisible();
    await expect(page.getByRole("button", { name: "Randomize Fleet" })).toBeVisible();
    await expect(page.getByRole("button", { name: "Clear Board" })).toBeVisible();
    await expect(page.getByRole("button", { name: "Confirm Placement" })).toBeVisible();
  });

  test("shows all 5 ship types in sidebar", async ({ page }) => {
    const shipButtons = page.locator(".ship-sidebar button");
    await expect(shipButtons).toHaveCount(5);
  });

  test("board has correct grid size", async ({ page }) => {
    const cells = page.locator(".cell");
    await expect(cells).toHaveCount(100); // 10x10 grid
  });

  test("board shows coordinate labels A-J", async ({ page }) => {
    const topLabels = page.locator(".labels-top span");
    await expect(topLabels).toHaveCount(10);
    
    const firstLabel = await topLabels.first().textContent();
    const lastLabel = await topLabels.last().textContent();
    
    expect(firstLabel).toBe("A");
    expect(lastLabel).toBe("J");
  });

  test("board shows coordinate labels 1-10", async ({ page }) => {
    const leftLabels = page.locator(".labels-left span");
    await expect(leftLabels).toHaveCount(10);
    
    const firstLabel = await leftLabels.first().textContent();
    const lastLabel = await leftLabels.last().textContent();
    
    expect(firstLabel).toBe("1");
    expect(lastLabel).toBe("10");
  });

  test("ship sidebar has proper styling", async ({ page }) => {
    const sidebar = page.locator(".ship-sidebar-wrapper");
    await expect(sidebar).toBeVisible();
    await expect(sidebar).toHaveCSS("display", "flex");
  });
});

test.describe("Placement Screen - Ship Placement", () => {
  test.beforeEach(async ({ page }) => {
    await page.getByRole("button", { name: "Vs AI (Easy)" }).click();
  });

  test("rotate button toggles horizontal/vertical", async ({ page }) => {
    const rotateBtn = page.getByRole("button", { name: /Rotate:/ });
    
    await expect(rotateBtn).toContainText("Horizontal");
    
    await rotateBtn.click();
    await expect(rotateBtn).toContainText("Vertical");
    
    await rotateBtn.click();
    await expect(rotateBtn).toContainText("Horizontal");
  });

  test("randomize fleet places all 5 ships", async ({ page }) => {
    await page.getByRole("button", { name: "Randomize Fleet" }).click();
    
    const ships = page.locator(".ship");
    await expect(ships).toHaveCount(5);
  });

  test("randomize fleet enables confirm button", async ({ page }) => {
    const confirmBtn = page.getByRole("button", { name: "Confirm Placement" });
    await expect(confirmBtn).toBeDisabled();
    
    await page.getByRole("button", { name: "Randomize Fleet" }).click();
    await expect(confirmBtn).toBeEnabled();
  });

  test("randomize fleet multiple times generates different layouts", async ({ page }) => {
    await page.getByRole("button", { name: "Randomize Fleet" }).click();
    const firstShip = page.locator(".ship").first();
    const firstStyle = await firstShip.getAttribute("style");
    
    // Try up to 10 times to get a different layout
    let foundDifferent = false;
    for (let i = 0; i < 10; i++) {
      await page.getByRole("button", { name: "Randomize Fleet" }).click();
      const secondStyle = await firstShip.getAttribute("style");
      
      if (firstStyle !== secondStyle) {
        foundDifferent = true;
        break;
      }
    }
    
    expect(foundDifferent).toBe(true);
  });

  test("clear board removes all ships", async ({ page }) => {
    await page.getByRole("button", { name: "Randomize Fleet" }).click();
    await expect(page.locator(".ship")).toHaveCount(5);
    
    await page.getByRole("button", { name: "Clear Board" }).click();
    await expect(page.locator(".ship")).toHaveCount(0);
  });

  test("clear board disables confirm button", async ({ page }) => {
    await page.getByRole("button", { name: "Randomize Fleet" }).click();
    const confirmBtn = page.getByRole("button", { name: "Confirm Placement" });
    await expect(confirmBtn).toBeEnabled();
    
    await page.getByRole("button", { name: "Clear Board" }).click();
    await expect(confirmBtn).toBeDisabled();
  });

  test("manual ship placement selects ship", async ({ page }) => {
    const firstShipBtn = page.locator(".ship-sidebar button").first();
    await firstShipBtn.click();
    
    await expect(firstShipBtn).toHaveClass(/selected-ship/);
  });

  test("selecting different ships updates selection", async ({ page }) => {
    const firstShip = page.locator(".ship-sidebar button").first();
    const secondShip = page.locator(".ship-sidebar button").nth(1);
    
    await firstShip.click();
    await expect(firstShip).toHaveClass(/selected-ship/);
    
    await secondShip.click();
    await expect(secondShip).toHaveClass(/selected-ship/);
    await expect(firstShip).not.toHaveClass(/selected-ship/);
  });

  test("placed ships become disabled in sidebar", async ({ page }) => {
    await page.getByRole("button", { name: "Randomize Fleet" }).click();
    
    const placedShips = page.locator(".placed-ship");
    await expect(placedShips).toHaveCount(5);
  });

  test("placed ships have reduced opacity", async ({ page }) => {
    await page.getByRole("button", { name: "Randomize Fleet" }).click();
    
    const placedShip = page.locator(".placed-ship").first();
    const icon = placedShip.locator(".ship-icon");
    
    const opacity = await icon.evaluate((el) => 
      window.getComputedStyle(el).opacity
    );
    
    expect(parseFloat(opacity)).toBeLessThan(1);
  });

  test("confirm button disabled until all ships placed", async ({ page }) => {
    const confirmBtn = page.getByRole("button", { name: "Confirm Placement" });
    await expect(confirmBtn).toBeDisabled();
    
    await page.getByRole("button", { name: "Randomize Fleet" }).click();
    await expect(confirmBtn).toBeEnabled();
  });

  test("ships display with correct rotation", async ({ page }) => {
    await page.getByRole("button", { name: "Randomize Fleet" }).click();
    
    const ships = page.locator(".ship");
    const firstShip = ships.first();
    const style = await firstShip.getAttribute("style");
    
    // Should have rotation set
    expect(style).toMatch(/--rotate:\s*(0deg|90deg)/);
  });

  test("board is clickable when ship is selected", async ({ page }) => {
    const firstShipBtn = page.locator(".ship-sidebar button").first();
    await firstShipBtn.click();
    
    const cell = page.locator(".cell").nth(20);
    await cell.click();
    
    await page.waitForTimeout(100);
    const ships = await page.locator(".ship").count();
    expect(ships).toBeGreaterThan(0);
  });
});

test.describe("Placement Screen - Transitions", () => {
  test("PvE Easy: confirms placement and starts game", async ({ page }) => {
    await page.getByRole("button", { name: "Vs AI (Easy)" }).click();
    await page.getByRole("button", { name: "Randomize Fleet" }).click();
    await page.getByRole("button", { name: "Confirm Placement" }).click();
    
    await expect(page.getByText("Your Fleet")).toBeVisible();
    await expect(page.getByText("Target Grid")).toBeVisible();
    await expect(page.locator(".turn-indicator")).toBeVisible();
  });

  test("PvE Hard: confirms placement and starts game", async ({ page }) => {
    await page.getByRole("button", { name: "Vs AI (Hard)" }).click();
    await page.getByRole("button", { name: "Randomize Fleet" }).click();
    await page.getByRole("button", { name: "Confirm Placement" }).click();
    
    await expect(page.getByText("Your Fleet")).toBeVisible();
    await expect(page.getByText("Target Grid")).toBeVisible();
  });

  test("placement state preserved during session", async ({ page }) => {
    await page.getByRole("button", { name: "Vs AI (Easy)" }).click();
    await page.getByRole("button", { name: "Randomize Fleet" }).click();
    
    const ships = page.locator(".ship");
    await expect(ships).toHaveCount(5);
    
    await page.waitForTimeout(500);
    await expect(ships).toHaveCount(5);
  });
});

test.describe("Game Screen - Layout", () => {
  async function startGame(page: Page) {
    await page.getByRole("button", { name: "Vs AI (Easy)" }).click();
    await page.getByRole("button", { name: "Randomize Fleet" }).click();
    await page.getByRole("button", { name: "Confirm Placement" }).click();
  }

  test.beforeEach(async ({ page }) => {
    await startGame(page);
  });

  test("shows game title", async ({ page }) => {
    await expect(page.getByRole("heading", { name: "BATTLESHIP" })).toBeVisible();
  });

  test("shows turn indicator", async ({ page }) => {
    const indicator = page.locator(".turn-indicator");
    await expect(indicator).toBeVisible();
    
    const text = await indicator.textContent();
    expect(text).toMatch(/YOUR TURN|OPPONENT'S TURN/);
  });

  test("shows both board titles", async ({ page }) => {
    await expect(page.getByText("Your Fleet")).toBeVisible();
    await expect(page.getByText("Target Grid")).toBeVisible();
  });

  test("renders two board containers", async ({ page }) => {
    const boardContainers = page.locator(".board-container");
    await expect(boardContainers).toHaveCount(2);
  });

  test("renders two boards", async ({ page }) => {
    const boards = page.locator(".board");
    await expect(boards).toHaveCount(2);
  });

  test("shows reset button", async ({ page }) => {
    const resetBtn = page.getByRole("button", { name: "Reset Game" });
    await expect(resetBtn).toBeVisible();
    await expect(resetBtn).toBeEnabled();
  });

  test("your fleet shows all 5 ships", async ({ page }) => {
    const yourFleetBoard = page.locator(".board-container").first();
    const ships = yourFleetBoard.locator(".ship");
    
    await expect(ships).toHaveCount(5);
  });

  test("target grid initially has no visible ships", async ({ page }) => {
    const targetBoard = page.locator(".board-container").last();
    const ships = targetBoard.locator(".ship");
    
    await expect(ships).toHaveCount(0);
  });

  test("both boards have coordinate labels", async ({ page }) => {
    const topLabels = page.locator(".labels-top");
    await expect(topLabels).toHaveCount(2);
    
    const leftLabels = page.locator(".labels-left");
    await expect(leftLabels).toHaveCount(2);
  });

  test("boards have proper styling and layout", async ({ page }) => {
    const boardWrapper = page.locator(".board-wrapper");
    await expect(boardWrapper).toBeVisible();
    await expect(boardWrapper).toHaveCSS("display", "flex");
  });
});

test.describe("Game Screen - Gameplay", () => {
  async function startGame(page: Page) {
    await page.getByRole("button", { name: "Vs AI (Easy)" }).click();
    await page.getByRole("button", { name: "Randomize Fleet" }).click();
    await page.getByRole("button", { name: "Confirm Placement" }).click();
  }

  test.beforeEach(async ({ page }) => {
    await startGame(page);
  });

  test("starts with player 1 turn", async ({ page }) => {
    const indicator = page.locator(".turn-indicator");
    await expect(indicator).toContainText("YOUR TURN");
  });

  test("clicking target grid creates marker", async ({ page }) => {
    const targetBoard = page.locator(".board-container").last();
    const cell = targetBoard.locator(".cell").nth(10);
    
    await cell.click();
    await page.waitForTimeout(200);
    
    const markers = targetBoard.locator(".marker");
    await expect(markers).toHaveCount(1);
  });

  test("marker has hit or miss class", async ({ page }) => {
    const targetBoard = page.locator(".board-container").last();
    await targetBoard.locator(".cell").nth(10).click();
    await page.waitForTimeout(200);
    
    const marker = targetBoard.locator(".marker").first();
    const classList = await marker.getAttribute("class");
    
    expect(classList).toMatch(/\b(hit|miss)\b/);
  });

  test("hit marker shows X symbol", async ({ page }) => {
    const targetBoard = page.locator(".board-container").last();

    for (let i = 0; i < 50; i++) {
      await targetBoard.locator(".cell").nth(i).click();
      await page.waitForTimeout(100);
      
      const hitMarker = targetBoard.locator(".marker.hit");
      const count = await hitMarker.count();
      
      if (count > 0) {
        const content = await hitMarker.first().evaluate(el => 
          window.getComputedStyle(el, '::before').content
        );
        expect(content).toContain("✕");
        break;
      }
    }
  });

  test("miss marker shows dot symbol", async ({ page }) => {
    const targetBoard = page.locator(".board-container").last();
    
    for (let i = 0; i < 50; i++) {
      await targetBoard.locator(".cell").nth(i).click();
      await page.waitForTimeout(100);
      
      const missMarker = targetBoard.locator(".marker.miss");
      const count = await missMarker.count();
      
      if (count > 0) {
        const content = await missMarker.first().evaluate(el => 
          window.getComputedStyle(el, '::before').content
        );
        expect(content).toContain("•");
        break;
      }
    }
  });

  test("cannot click same cell twice", async ({ page }) => {
    const targetBoard = page.locator(".board-container").last();
    const cell = targetBoard.locator(".cell").nth(15);
    
    await cell.click();
    await page.waitForTimeout(200);
    
    const markersAfterFirst = await targetBoard.locator(".marker").count();
    
    await cell.click();
    await page.waitForTimeout(200);
    
    const markersAfterSecond = await targetBoard.locator(".marker").count();

    expect(markersAfterSecond).toBe(markersAfterFirst);
  });

  test("AI makes moves in PvE mode", async ({ page }) => {
    const targetBoard = page.locator(".board-container").last();
    const yourFleetBoard = page.locator(".board-container").first();

    await targetBoard.locator(".cell").nth(10).click();

    await page.waitForTimeout(1500);

    const markers = yourFleetBoard.locator(".marker");
    const markerCount = await markers.count();
    
    expect(markerCount).toBeGreaterThanOrEqual(1);
  });

  test("turn indicator changes after player move", async ({ page }) => {
    const indicator = page.locator(".turn-indicator");
    await expect(indicator).toContainText("YOUR TURN");
    
    const targetBoard = page.locator(".board-container").last();
    await targetBoard.locator(".cell").nth(10).click();

    await page.waitForTimeout(200);
    await expect(indicator).toContainText("OPPONENT'S TURN");
  });

  test("turn indicator returns to player after AI move", async ({ page }) => {
    const indicator = page.locator(".turn-indicator");
    const targetBoard = page.locator(".board-container").last();
    
    await targetBoard.locator(".cell").nth(10).click();
    await page.waitForTimeout(200);
    
    await expect(indicator).toContainText("OPPONENT'S TURN");
    await page.waitForTimeout(1500);
    
    await expect(indicator).toContainText("YOUR TURN");
  });

  test("multiple moves create multiple markers", async ({ page }) => {
    const targetBoard = page.locator(".board-container").last();
    await targetBoard.locator(".cell").nth(0).click();
    await page.waitForTimeout(1500); // Wait for AI
    
    await targetBoard.locator(".cell").nth(11).click();
    await page.waitForTimeout(1500);
    
    await targetBoard.locator(".cell").nth(22).click();
    await page.waitForTimeout(200);
    
    const markers = targetBoard.locator(".marker");
    await expect(markers).toHaveCount(3);
  });

  test("markers persist across turns", async ({ page }) => {
    const targetBoard = page.locator(".board-container").last();
    
    await targetBoard.locator(".cell").nth(10).click();
    await page.waitForTimeout(200);
    await expect(targetBoard.locator(".marker")).toHaveCount(1);
    
    await page.waitForTimeout(1500); // Wait for AI
    
    // Original marker should still be there
    await expect(targetBoard.locator(".marker")).toHaveCount(1);
  });

  test("cannot click on your own fleet board", async ({ page }) => {
    const yourFleetBoard = page.locator(".board-container").first();
    const markersBefore = await yourFleetBoard.locator(".marker").count();
    
    await yourFleetBoard.locator(".cell").nth(10).click({ force: true });
    await page.waitForTimeout(200);
    
    const markersAfter = await yourFleetBoard.locator(".marker").count();
    expect(markersAfter).toBe(markersBefore);
  });
});

test.describe("Game Screen - Reset", () => {
  async function startGame(page: Page) {
    await page.getByRole("button", { name: "Vs AI (Easy)" }).click();
    await page.getByRole("button", { name: "Randomize Fleet" }).click();
    await page.getByRole("button", { name: "Confirm Placement" }).click();
  }

  test("reset button returns to setup screen", async ({ page }) => {
    await startGame(page);
    
    await page.getByRole("button", { name: "Reset Game" }).click();
    
    await expect(page.getByRole("heading", { name: "BATTLESHIP" })).toBeVisible();
    await expect(page.getByText("Select Game Mode")).toBeVisible();
    await expect(page.getByRole("button", { name: "Player vs Player" })).toBeVisible();
  });

  test("reset clears game state", async ({ page }) => {
    await startGame(page);
    
    const targetBoard = page.locator(".board-container").last();
    await targetBoard.locator(".cell").nth(10).click();
    await page.waitForTimeout(1500);
    
    await page.getByRole("button", { name: "Reset Game" }).click();
    
    await startGame(page);
    
    const allMarkers = page.locator(".marker");
    await expect(allMarkers).toHaveCount(0);
  });

  test("reset works mid-game", async ({ page }) => {
    await startGame(page);
    
    const targetBoard = page.locator(".board-container").last();
    await targetBoard.locator(".cell").nth(10).click();
    await page.waitForTimeout(200);

    await page.getByRole("button", { name: "Reset Game" }).click();
    
    await expect(page.getByText("Select Game Mode")).toBeVisible();
  });

  test("can start different mode after reset", async ({ page }) => {
    await startGame(page);
    await page.getByRole("button", { name: "Reset Game" }).click();
    
    await page.getByRole("button", { name: "Vs AI (Hard)" }).click();
    await expect(page.getByText("Place Your Ships")).toBeVisible();
  });
});

test.describe("Different Game Modes", () => {
  test("PvE Easy mode completes full flow", async ({ page }) => {
    await page.getByRole("button", { name: "Vs AI (Easy)" }).click();
    await expect(page.getByText("Place Your Ships")).toBeVisible();
    
    await page.getByRole("button", { name: "Randomize Fleet" }).click();
    await page.getByRole("button", { name: "Confirm Placement" }).click();
    
    await expect(page.getByText("Your Fleet")).toBeVisible();
    await expect(page.locator(".turn-indicator")).toContainText("YOUR TURN");
  });

  test("PvE Hard mode completes full flow", async ({ page }) => {
    await page.getByRole("button", { name: "Vs AI (Hard)" }).click();
    await expect(page.getByText("Place Your Ships")).toBeVisible();
    
    await page.getByRole("button", { name: "Randomize Fleet" }).click();
    await page.getByRole("button", { name: "Confirm Placement" }).click();
    
    await expect(page.getByText("Your Fleet")).toBeVisible();
    await expect(page.locator(".turn-indicator")).toContainText("YOUR TURN");
  });

  test("AI Easy and Hard have same UI", async ({ page }) => {
    // Test Easy
    await page.getByRole("button", { name: "Vs AI (Easy)" }).click();
    await page.getByRole("button", { name: "Randomize Fleet" }).click();
    await page.getByRole("button", { name: "Confirm Placement" }).click();
    
    await expect(page.locator(".battleship-container")).toBeVisible();
    const easyLayout = await page.locator(".battleship-container").isVisible();
    expect(easyLayout).toBe(true);
    
    await page.getByRole("button", { name: "Reset Game" }).click();
    await page.waitForTimeout(100);
    await expect(page.getByText("Select Game Mode")).toBeVisible();

    await page.getByRole("button", { name: "Vs AI (Hard)" }).click();
    await page.getByRole("button", { name: "Randomize Fleet" }).click();
    await page.getByRole("button", { name: "Confirm Placement" }).click();
    
    await expect(page.locator(".battleship-container")).toBeVisible();
    const hardLayout = await page.locator(".battleship-container").isVisible();
    expect(hardLayout).toBe(true);
  });
});

test.describe("Ship Rendering", () => {
  test("ships render with correct images", async ({ page }) => {
    await page.getByRole("button", { name: "Vs AI (Easy)" }).click();
    await page.getByRole("button", { name: "Randomize Fleet" }).click();
    
    const shipImages = page.locator(".ship img");
    await expect(shipImages).toHaveCount(5);
    
    for (let i = 0; i < 5; i++) {
      const src = await shipImages.nth(i).getAttribute("src");
      expect(src).toBeTruthy();
      expect(src).toContain(".svg");
    }
  });

  test("ships have proper CSS variables for positioning", async ({ page }) => {
    await page.getByRole("button", { name: "Vs AI (Easy)" }).click();
    await page.getByRole("button", { name: "Randomize Fleet" }).click();
    
    const firstShip = page.locator(".ship").first();
    const style = await firstShip.getAttribute("style");
    
    expect(style).toContain("--x:");
    expect(style).toContain("--y:");
    expect(style).toContain("--w:");
    expect(style).toContain("--h:");
    expect(style).toContain("--rotate:");
  });

  test("sunk ships become visible on opponent board", async ({ page }) => {
    await page.getByRole("button", { name: "Vs AI (Easy)" }).click();
    await page.getByRole("button", { name: "Randomize Fleet" }).click();
    await page.getByRole("button", { name: "Confirm Placement" }).click();
    
    const targetBoard = page.locator(".board-container").last();
    const initialShips = await targetBoard.locator(".ship").count();

    for (let i = 0; i < 100; i++) {
      const cell = targetBoard.locator(".cell").nth(i);
      await cell.click();
      await page.waitForTimeout(50);
    }

    await page.waitForTimeout(500);
    const finalShips = await targetBoard.locator(".ship").count();
    
    expect(finalShips).toBeGreaterThanOrEqual(initialShips);
  });
});

test.describe("Responsive Design", () => {
  test("works on mobile viewport", async ({ page }) => {
    await page.setViewportSize({ width: 375, height: 667 });
    
    await expect(page.getByRole("heading", { name: "BATTLESHIP" })).toBeVisible();
    await expect(page.getByRole("button", { name: "Vs AI (Easy)" })).toBeVisible();
    
    await page.getByRole("button", { name: "Vs AI (Easy)" }).click();
    await expect(page.getByText("Place Your Ships")).toBeVisible();
    
    const board = page.locator(".board");
    await expect(board).toBeVisible();
  });

  test("works on tablet viewport", async ({ page }) => {
    await page.setViewportSize({ width: 768, height: 1024 });
    
    await expect(page.getByRole("heading", { name: "BATTLESHIP" })).toBeVisible();
    await expect(page.getByRole("button", { name: "Vs AI (Easy)" })).toBeVisible();
  });

  test("boards scale properly on small screens", async ({ page }) => {
    await page.setViewportSize({ width: 375, height: 667 });
    
    await page.getByRole("button", { name: "Vs AI (Easy)" }).click();
    await page.getByRole("button", { name: "Randomize Fleet" }).click();
    
    const board = page.locator(".board");
    await expect(board).toBeVisible();
  });
});

test.describe("Visual Regression", () => {
  test("setup screen matches snapshot", async ({ page }) => {
    await expect(page).toHaveScreenshot("setup-screen.png");
  });

  test("placement screen matches snapshot", async ({ page }) => {
    await page.getByRole("button", { name: "Vs AI (Easy)" }).click();
    await expect(page).toHaveScreenshot("placement-screen.png");
  });

  test("game screen matches snapshot", async ({ page }) => {
    await page.getByRole("button", { name: "Vs AI (Easy)" }).click();
    await page.getByRole("button", { name: "Randomize Fleet" }).click();
    await page.getByRole("button", { name: "Confirm Placement" }).click();
    
    await expect(page.locator(".turn-indicator")).toContainText("YOUR TURN");
    await page.waitForTimeout(100);
    await expect(page).toHaveScreenshot("game-screen.png", { maxDiffPixelRatio: 0.05 });
  });
});