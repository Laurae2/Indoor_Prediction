/*
  Licensed under the Apache License, Version 2.0
    http://www.apache.org/licenses/LICENSE-2.0.html

  AUTOGENERATED BY H2O at 2016-12-26T13:51:31.358+01:00
  3.10.0.8
  
  Standalone prediction code with sample test data for GLMModel named h2o_glm_08

  How to download, compile and execute:
      mkdir tmpdir
      cd tmpdir
      curl http://127.0.0.1:54321/3/h2o-genmodel.jar > h2o-genmodel.jar
      curl http://127.0.0.1:54321/3/Models.java/h2o_glm_08 > h2o_glm_08.java
      javac -cp h2o-genmodel.jar -J-Xmx2g -J-XX:MaxPermSize=128m h2o_glm_08.java

     (Note:  Try java argument -XX:+PrintCompilation to show runtime JIT compiler behavior.)
*/
import java.util.Map;
import hex.genmodel.GenModel;
import hex.genmodel.annotations.ModelPojo;

@ModelPojo(name="h2o_glm_08", algorithm="glm")
public class h2o_glm_08 extends GenModel {
  public hex.ModelCategory getModelCategory() { return hex.ModelCategory.Multinomial; }

  public boolean isSupervised() { return true; }
  public int nfeatures() { return 36; }
  public int nclasses() { return 6; }

  // Names of columns used by model.
  public static final String[] NAMES = NamesHolder_h2o_glm_08.VALUES;
  // Number of output classes included in training data response column.
  public static final int NCLASSES = 6;

  // Column domains. The last array contains domain of response column.
  public static final String[][] DOMAINS = new String[][] {
    /* X1 */ null,
    /* X2 */ null,
    /* X3 */ null,
    /* X4 */ null,
    /* X5 */ null,
    /* X6 */ null,
    /* X7 */ null,
    /* X8 */ null,
    /* X9 */ null,
    /* X10 */ null,
    /* X11 */ null,
    /* X12 */ null,
    /* X13 */ null,
    /* X14 */ null,
    /* X15 */ null,
    /* X16 */ null,
    /* X17 */ null,
    /* X18 */ null,
    /* X19 */ null,
    /* X20 */ null,
    /* X21 */ null,
    /* X22 */ null,
    /* X23 */ null,
    /* X24 */ null,
    /* X25 */ null,
    /* X26 */ null,
    /* X27 */ null,
    /* X28 */ null,
    /* X29 */ null,
    /* X30 */ null,
    /* X31 */ null,
    /* X32 */ null,
    /* X33 */ null,
    /* X34 */ null,
    /* X35 */ null,
    /* X36 */ null,
    /* Label */ h2o_glm_08_ColInfo_36.VALUES
  };
  // Prior class distribution
  public static final double[] PRIOR_CLASS_DISTRIB = {0.24528301886792453,0.12264150943396226,0.12264150943396226,0.1320754716981132,0.24528301886792453,0.1320754716981132};
  // Class distribution used for model building
  public static final double[] MODEL_CLASS_DISTRIB = null;

  public h2o_glm_08() { super(NAMES,DOMAINS); }
  public String getUUID() { return Long.toString(2093725610206890812L); }

  // Pass in data in a double[], pre-aligned to the Model's requirements.
  // Jam predictions into the preds[] array; preds[0] is reserved for the
  // main prediction (class for classifiers or value for regression),
  // and remaining columns hold a probability distribution for classifiers.
  public final double[] score0( double[] data, double[] preds ) {
    final double [] b = BETA.VALUES;
    for(int i = 0; i < 0; ++i) if(Double.isNaN(data[i])) data[i] = CAT_MODES.VALUES[i];
    for(int i = 0; i < 36; ++i) if(Double.isNaN(data[i + 0])) data[i+0] = NUM_MEANS.VALUES[i];
    preds[0] = 0;
    for(int c = 0; c < 6; ++c){
      preds[c+1] = 0;
      for(int i = 0; i < 36; ++i)
        preds[c+1] += b[0+i + c*37]*data[i];
      preds[c+1] += b[36 + c*37]; // reduce intercept
    }
    double max_row = 0;
    for(int c = 1; c < preds.length; ++c) if(preds[c] > max_row) max_row = preds[c];
    double sum_exp = 0;
    for(int c = 1; c < preds.length; ++c) { sum_exp += (preds[c] = Math.exp(preds[c]-max_row));}
    sum_exp = 1/sum_exp;
    double max_p = 0;
    for(int c = 1; c < preds.length; ++c) if((preds[c] *= sum_exp) > max_p){ max_p = preds[c]; preds[0] = c-1;};
    return preds;
  }
    public static class BETA implements java.io.Serializable {
      public static final double[] VALUES = new double[222];
      static {
        BETA_0.fill(VALUES);
      }
      static final class BETA_0 implements java.io.Serializable {
        static final void fill(double[] sa) {
          sa[0] = 0.0;
          sa[1] = 0.0;
          sa[2] = 0.0;
          sa[3] = 0.042920936699808565;
          sa[4] = 0.0;
          sa[5] = 0.0;
          sa[6] = 0.0;
          sa[7] = 0.0;
          sa[8] = 0.0;
          sa[9] = -0.6319202690053864;
          sa[10] = 0.0;
          sa[11] = 0.0;
          sa[12] = 0.0;
          sa[13] = 0.0;
          sa[14] = 0.0;
          sa[15] = 0.0;
          sa[16] = 0.0;
          sa[17] = -0.21951767444150894;
          sa[18] = 0.0;
          sa[19] = 0.0;
          sa[20] = 0.0;
          sa[21] = 0.0;
          sa[22] = 0.0;
          sa[23] = 0.0;
          sa[24] = 0.0;
          sa[25] = 0.0;
          sa[26] = 0.0;
          sa[27] = 0.0;
          sa[28] = 0.0;
          sa[29] = 0.0;
          sa[30] = 0.0;
          sa[31] = 0.0;
          sa[32] = 0.9626284491108819;
          sa[33] = 0.0;
          sa[34] = -0.5016693854404196;
          sa[35] = -0.32651401194797547;
          sa[36] = -1.5484822240585256;
          sa[37] = 0.0;
          sa[38] = 0.0;
          sa[39] = 0.0;
          sa[40] = 0.0;
          sa[41] = 0.0;
          sa[42] = 0.0;
          sa[43] = 0.0;
          sa[44] = 0.0;
          sa[45] = 0.0;
          sa[46] = 0.0;
          sa[47] = 0.0;
          sa[48] = 0.0;
          sa[49] = 0.0;
          sa[50] = 0.0;
          sa[51] = 0.0;
          sa[52] = 0.0;
          sa[53] = 0.0;
          sa[54] = 0.0;
          sa[55] = 0.0;
          sa[56] = -0.8601710865496754;
          sa[57] = 0.0;
          sa[58] = 0.0;
          sa[59] = 0.0;
          sa[60] = 0.0;
          sa[61] = 0.0;
          sa[62] = 0.0;
          sa[63] = 0.0;
          sa[64] = 0.0;
          sa[65] = 0.0;
          sa[66] = 0.0;
          sa[67] = 0.0;
          sa[68] = 0.0;
          sa[69] = 0.3897222499808025;
          sa[70] = 0.4024211113732088;
          sa[71] = -0.006261974749095844;
          sa[72] = -0.4376657082950134;
          sa[73] = -1.9048867454136305;
          sa[74] = 0.0;
          sa[75] = 0.0;
          sa[76] = 0.0;
          sa[77] = 0.0;
          sa[78] = 0.0;
          sa[79] = 0.0;
          sa[80] = 0.0;
          sa[81] = 0.0;
          sa[82] = 0.0;
          sa[83] = 0.0;
          sa[84] = 0.0;
          sa[85] = 1.2335705306618505;
          sa[86] = 0.0;
          sa[87] = 0.0;
          sa[88] = 0.0;
          sa[89] = 0.0;
          sa[90] = 0.0;
          sa[91] = 0.0;
          sa[92] = 0.0;
          sa[93] = 0.0;
          sa[94] = 0.0;
          sa[95] = 0.0;
          sa[96] = 0.0;
          sa[97] = 0.0;
          sa[98] = 0.0;
          sa[99] = 0.0;
          sa[100] = 0.0;
          sa[101] = 0.0;
          sa[102] = 0.0;
          sa[103] = 0.0;
          sa[104] = 0.0;
          sa[105] = 0.0;
          sa[106] = 0.0;
          sa[107] = 0.8241205152415901;
          sa[108] = -0.7836032684102875;
          sa[109] = 0.0;
          sa[110] = -1.8317099342032261;
          sa[111] = 0.0;
          sa[112] = 0.0;
          sa[113] = 0.0;
          sa[114] = 0.0;
          sa[115] = 0.0;
          sa[116] = 0.0;
          sa[117] = 0.0;
          sa[118] = 0.0;
          sa[119] = 0.36990387066596425;
          sa[120] = 0.0;
          sa[121] = 0.0;
          sa[122] = 0.0;
          sa[123] = 0.0;
          sa[124] = 0.0;
          sa[125] = 0.0;
          sa[126] = 0.0;
          sa[127] = 0.3581742344337536;
          sa[128] = 0.0;
          sa[129] = 0.0;
          sa[130] = 0.5816161652639882;
          sa[131] = 0.0;
          sa[132] = 0.0;
          sa[133] = 0.0;
          sa[134] = 0.0;
          sa[135] = 0.0;
          sa[136] = 0.0;
          sa[137] = 0.0;
          sa[138] = 0.0;
          sa[139] = 0.0;
          sa[140] = 0.0;
          sa[141] = 0.0;
          sa[142] = 0.0;
          sa[143] = -0.03603728853400348;
          sa[144] = -0.9987189753622864;
          sa[145] = 0.0;
          sa[146] = 0.0;
          sa[147] = -1.8607317832561667;
          sa[148] = 0.0;
          sa[149] = 0.0;
          sa[150] = 0.0;
          sa[151] = 0.0;
          sa[152] = 0.0;
          sa[153] = 0.0;
          sa[154] = 0.0;
          sa[155] = 0.0;
          sa[156] = 0.0;
          sa[157] = 0.0;
          sa[158] = -0.2926259898824879;
          sa[159] = 0.0;
          sa[160] = 0.0;
          sa[161] = 0.0;
          sa[162] = 0.0;
          sa[163] = 0.0;
          sa[164] = 0.0;
          sa[165] = 0.0;
          sa[166] = 0.3595061713342933;
          sa[167] = 0.0;
          sa[168] = 0.0;
          sa[169] = 0.0;
          sa[170] = 0.0;
          sa[171] = 0.0;
          sa[172] = 0.0;
          sa[173] = -0.43080108864574496;
          sa[174] = 0.0;
          sa[175] = 0.0;
          sa[176] = 0.0;
          sa[177] = 0.0;
          sa[178] = 0.0;
          sa[179] = 0.0;
          sa[180] = 0.0;
          sa[181] = -0.43968737411481656;
          sa[182] = 1.21283107668429;
          sa[183] = 0.7569608863658416;
          sa[184] = -1.5573646425261398;
          sa[185] = 0.0;
          sa[186] = 0.0;
          sa[187] = 0.0;
          sa[188] = 0.0;
          sa[189] = 0.0;
          sa[190] = 0.0;
          sa[191] = 0.0;
          sa[192] = 0.0;
          sa[193] = 0.0;
          sa[194] = 0.0;
          sa[195] = 0.22291241117191762;
          sa[196] = -0.3239951598540039;
          sa[197] = 0.0;
          sa[198] = 0.0;
          sa[199] = 0.0;
          sa[200] = 0.0;
          sa[201] = 0.0;
          sa[202] = 0.0;
          sa[203] = 0.0;
          sa[204] = 0.0;
          sa[205] = 0.0;
          sa[206] = 0.0;
          sa[207] = 0.0;
          sa[208] = 0.0;
          sa[209] = 0.0;
          sa[210] = 0.16336364533374725;
          sa[211] = 0.0;
          sa[212] = 1.5580305327139636;
          sa[213] = 0.0;
          sa[214] = 0.0;
          sa[215] = 0.0;
          sa[216] = 0.0;
          sa[217] = -0.29652072124704376;
          sa[218] = 0.0;
          sa[219] = 0.34298204305698865;
          sa[220] = 0.292115844333991;
          sa[221] = -1.9294552975568187;
        }
      }
}
// Imputed numeric values
    static class NUM_MEANS implements java.io.Serializable {
      public static final double[] VALUES = new double[36];
      static {
        NUM_MEANS_0.fill(VALUES);
      }
      static final class NUM_MEANS_0 implements java.io.Serializable {
        static final void fill(double[] sa) {
          sa[0] = 0.16269148055293456;
          sa[1] = -0.1873074558500091;
          sa[2] = -0.10801097846989315;
          sa[3] = -0.043147932169831746;
          sa[4] = 0.13289582332404562;
          sa[5] = 0.14252046456356515;
          sa[6] = 0.1532462497674318;
          sa[7] = 0.08670248875371696;
          sa[8] = 0.25239599040525224;
          sa[9] = -0.2239265353127661;
          sa[10] = -0.2335142794752041;
          sa[11] = -0.21432588409085435;
          sa[12] = 0.17059887858954062;
          sa[13] = 0.1616007312366226;
          sa[14] = 0.16740760283984527;
          sa[15] = 0.08516193152739539;
          sa[16] = -0.2406530780711871;
          sa[17] = -0.20879022991702365;
          sa[18] = 0.25691536440336205;
          sa[19] = -0.1726023939589246;
          sa[20] = 0.16222407386792506;
          sa[21] = 0.14088990530443604;
          sa[22] = 0.15952512463493737;
          sa[23] = 0.08186644285050128;
          sa[24] = -0.09308371314757652;
          sa[25] = -0.15658572686021255;
          sa[26] = 0.1713188059845201;
          sa[27] = 0.053149357129114345;
          sa[28] = 0.15863405154813573;
          sa[29] = 0.1314584465052417;
          sa[30] = 0.1453468069041645;
          sa[31] = 0.08222365628912942;
          sa[32] = -0.031865801886792514;
          sa[33] = -0.17368165094339627;
          sa[34] = -0.16938239622641513;
          sa[35] = -0.05929948113207548;
        }
      }
}
// Imputed categorical values.
    static class CAT_MODES implements java.io.Serializable {
      public static final int[] VALUES = new int[0];
      static {
      }
}
    // Categorical Offsets
    public static final int[] CATOFFS = {0};
}
// The class representing training column names
class NamesHolder_h2o_glm_08 implements java.io.Serializable {
  public static final String[] VALUES = new String[36];
  static {
    NamesHolder_h2o_glm_08_0.fill(VALUES);
  }
  static final class NamesHolder_h2o_glm_08_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "X1";
      sa[1] = "X2";
      sa[2] = "X3";
      sa[3] = "X4";
      sa[4] = "X5";
      sa[5] = "X6";
      sa[6] = "X7";
      sa[7] = "X8";
      sa[8] = "X9";
      sa[9] = "X10";
      sa[10] = "X11";
      sa[11] = "X12";
      sa[12] = "X13";
      sa[13] = "X14";
      sa[14] = "X15";
      sa[15] = "X16";
      sa[16] = "X17";
      sa[17] = "X18";
      sa[18] = "X19";
      sa[19] = "X20";
      sa[20] = "X21";
      sa[21] = "X22";
      sa[22] = "X23";
      sa[23] = "X24";
      sa[24] = "X25";
      sa[25] = "X26";
      sa[26] = "X27";
      sa[27] = "X28";
      sa[28] = "X29";
      sa[29] = "X30";
      sa[30] = "X31";
      sa[31] = "X32";
      sa[32] = "X33";
      sa[33] = "X34";
      sa[34] = "X35";
      sa[35] = "X36";
    }
  }
}
// The class representing column Label
class h2o_glm_08_ColInfo_36 implements java.io.Serializable {
  public static final String[] VALUES = new String[6];
  static {
    h2o_glm_08_ColInfo_36_0.fill(VALUES);
  }
  static final class h2o_glm_08_ColInfo_36_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "1";
      sa[1] = "2";
      sa[2] = "3";
      sa[3] = "4";
      sa[4] = "5";
      sa[5] = "6";
    }
  }
}


