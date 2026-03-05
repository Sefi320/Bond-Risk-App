# Bond-Risk-App

# Bond Portfolio Risk Analyzer

A containerized **Shiny application for pricing bond portfolios and analyzing interest rate risk** using Treasury yield curves.

The application allows users to construct a portfolio of coupon bonds, generate a zero-rate curve, and compute key interest-rate risk metrics including **delta, DV01, and gamma**.

***The application is designed as an educational tool for curios minds***.

------------------------------------------------------------------------

# Key Capabilities

The application provides the following functionality.

## Portfolio Pricing

-   Bond cash flow generation
-   Discounting using a zero-rate curve
-   Portfolio valuation

## Interest Rate Risk Analytics

-   Delta (numerical using the Central Difference Method)
-   DV01 (Dollar Value of a change in interest rates by 1bp)
-   Dollar gamma (numerical using the Central Difference Method)

## Risk Decomposition

-   Portfolio DV01 contribution analysis
-   Interactive visualization of risk concentrations

## Visualization

-   Interactive charts built with `plotly`
-   Yield curve visualization
-   Yield curve risk metrics visualization

------------------------------------------------------------------------

# Methodology

## Bond Pricing

Each bond is priced using discounted cash flows:

$$
P = \sum_{t=1}^{T} CF_t \times DF_t
$$

Portfolio value is the sum of the present values of all bonds.

------------------------------------------------------------------------

## Yield Curve Construction

The yield curve is built from **Treasury Constant Maturity (CMT) data**.

------------------------------------------------------------------------

## Duration

Modified duration is computed numerically using the **central difference method**:

This measures **portfolio value sensitivity to interest rate changes**.

------------------------------------------------------------------------

## DV01

DV01 represents the **dollar change in portfolio value for a 1 basis point change in rates**.

It is calculated by repricing the portfolio under ±1 basis point parallel shifts.

------------------------------------------------------------------------

## Gamma

This captures the **curvature of the price–yield relationship**.

------------------------------------------------------------------------

## DV01 Contribution Analysis

Bond-level DV01 is computed individually and visualized as the % each bond contributes to the portfolio's DV01

------------------------------------------------------------------------

# Docker Deployment

The application is designed to run in **isolated Docker containers**.

------------------------------------------------------------------------

## Build the Docker Image

From the repository root:

``` bash
docker build -t Bond-Risk-App .
```

------------------------------------------------------------------------

## Run the Container

``` bash
docker run -p 3838:3838 Bond-Risk-app
```

The application will be available at

```         
http://localhost:3838
```

------------------------------------------------------------------------

# Example Workflow

1.  Launch the application

2.  Explore and compare historical risk metrics of each CMT from 1992-Today

3.  Explore the term structure of a date of your choosing and see the zero curve being built in real-time by log-linear inerpolation of the discount factors

4.  Enter bonds into the portfolio input table

5.  Select the portfolio valuation date (If the app returns an error, select a different valuation date because it may just mean that there is no yield data for the selected day)

The application automatically:

-   builds the zero curve
-   prices the portfolio by discounting the cash flows of each bond and aggregating the PV of all of the CFs
-   computes duration, DV01, and gamma

Review the **DV01 contribution chart** to identify risk concentrations.

------------------------------------------------------------------------

# Intended Audience

This application is intended for educational purposes only, it is not meant to be used as a primary trading analytics tool.

-   Curios Minds

------------------------------------------------------------------------

# Future Extensions

Potential improvements include

-   key rate duration
-   yield curve non-parallel shocks

------------------------------------------------------------------------
