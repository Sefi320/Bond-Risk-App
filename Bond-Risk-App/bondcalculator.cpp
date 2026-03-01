#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

// [[Rcpp::export]]
double bond_price(double maturity = 2, double rate = 0.05, double coupon = 0.1, int m = 2, int face = 100) {
  double price = 0.0;
  int periods = round(m * maturity);
  for (int i = 1; i <= periods; ++i) {
    price += (coupon * face / m) / pow((1 + rate / m), i);
  }
  return(price += face / pow(1 + rate / m, periods));
}

// [[Rcpp::export]]
double bond_duration(double maturity = 2, double rate = 0.05, double coupon = 0.1, int m = 2, int face = 100) {
  double bond_npv = 0.0;
  int periods = round(m * maturity);
  for (int i = 1; i <= periods; ++i) {
    bond_npv += i * (coupon * face / m) / pow(1 + rate / m, i);
  }
  bond_npv += periods * face / pow(1 + rate / m, periods);
  return((bond_npv / bond_price(maturity, rate, coupon, m, face)) / m);
}

// [[Rcpp::export]]
double bond_convexity(double maturity = 2, double rate = 0.05, double coupon = 0.1, int m = 2, int face = 100) {
  double bond_convex = 0.0;
  int periods = round(m * maturity);
  for (int i = 1; i <= periods; ++i) {
    bond_convex += (coupon * face / m) * (pow(i, 2) + i) / (pow(m, 2) * pow(1 + rate / m, i)); 
  }
  bond_convex += face * (pow(periods, 2) + periods) / (pow(m, 2) * pow(1 + rate / m, periods));
  return(bond_convex / (bond_price(maturity, rate, coupon, m, face) * pow(1 + rate / m, 2)));
}