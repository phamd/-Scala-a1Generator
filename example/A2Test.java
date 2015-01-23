import static org.junit.Assert.*;

import org.junit.BeforeClass;
import org.junit.Test;

import se2s03.A2;

public class A2Test {
    private static A2 tester;
    
    @BeforeClass
    public static void setUp(){
        tester = new A2();
    }
    
    @Test
    public void RecTest() {
        assertEquals("Rec(0) must be 3", 3, tester.Rec(0));
        assertEquals("Rec(1) must be -2", -2, tester.Rec(1));
        assertEquals("Rec(2) must be 8", 8, tester.Rec(2));
        assertEquals("Rec(3) must be -12", -12, tester.Rec(3));
        assertEquals("Rec(4) must be 28", 28, tester.Rec(4));
        assertEquals("Rec(5) must be -52", -52, tester.Rec(5));
        assertEquals("Rec(6) must be 108", 108, tester.Rec(6));
        assertEquals("Rec(7) must be -212", -212, tester.Rec(7));
        assertEquals("Rec(8) must be 428", 428, tester.Rec(8));
        assertEquals("Rec(9) must be -852", -852, tester.Rec(9));
        assertEquals("Rec(10) must be 1708", 1708, tester.Rec(10));
        assertEquals("Rec(11) must be -3412", -3412, tester.Rec(11));
        assertEquals("Rec(12) must be 6828", 6828, tester.Rec(12));
        assertEquals("Rec(13) must be -13652", -13652, tester.Rec(13));
        assertEquals("Rec(14) must be 27308", 27308, tester.Rec(14));
        assertEquals("Rec(15) must be -54612", -54612, tester.Rec(15));
        assertEquals("Rec(16) must be 109228", 109228, tester.Rec(16));
        assertEquals("Rec(17) must be -218452", -218452, tester.Rec(17));
        assertEquals("Rec(18) must be 436908", 436908, tester.Rec(18));
        assertEquals("Rec(19) must be -873812", -873812, tester.Rec(19));
        assertEquals("Rec(20) must be 1747628", 1747628, tester.Rec(20));
        assertEquals("Rec(21) must be -3495252", -3495252, tester.Rec(21));
        assertEquals("Rec(22) must be 6990508", 6990508, tester.Rec(22));
        assertEquals("Rec(23) must be -13981012", -13981012, tester.Rec(23));
        assertEquals("Rec(24) must be 27962028", 27962028, tester.Rec(24));
        assertEquals("Rec(25) must be -55924052", -55924052, tester.Rec(25));
        assertEquals("Rec(26) must be 111848108", 111848108, tester.Rec(26));
        assertEquals("Rec(27) must be -223696212", -223696212, tester.Rec(27));
        assertEquals("Rec(28) must be 447392428", 447392428, tester.Rec(28));
        assertEquals("Rec(29) must be -894784852", -894784852, tester.Rec(29));
        assertEquals("Rec(30) must be 1789569708", 1789569708, tester.Rec(30));
    }
}
