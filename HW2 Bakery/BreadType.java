/*
    Justin Chen
    I pledge my honor that I have abided by the Stevens Honor System
*/

public enum BreadType {
    RYE (3.99f),
    SOURDOUGH (4.99f),
    WONDER (5.99f);

    private float price;

    BreadType(float price) {
        this.price = price;
    }

    /**
     * Gets the price of a piece of bread
     * @return Price of bread
     */
    public float getPrice() {
        return price;
    }
}