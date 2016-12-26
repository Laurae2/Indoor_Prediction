/*
  Licensed under the Apache License, Version 2.0
    http://www.apache.org/licenses/LICENSE-2.0.html

  AUTOGENERATED BY H2O at 2016-12-26T14:42:09.400+01:00
  3.10.0.8
  
  Standalone prediction code with sample test data for DeepLearningModel named h2o_nn_32x6_Tanh_06

  How to download, compile and execute:
      mkdir tmpdir
      cd tmpdir
      curl http://127.0.0.1:54321/3/h2o-genmodel.jar > h2o-genmodel.jar
      curl http://127.0.0.1:54321/3/Models.java/h2o_nn_32x6_Tanh_06 > h2o_nn_32x6_Tanh_06.java
      javac -cp h2o-genmodel.jar -J-Xmx2g -J-XX:MaxPermSize=128m h2o_nn_32x6_Tanh_06.java

     (Note:  Try java argument -XX:+PrintCompilation to show runtime JIT compiler behavior.)
*/
import java.util.Map;
import hex.genmodel.GenModel;
import hex.genmodel.annotations.ModelPojo;

@ModelPojo(name="h2o_nn_32x6_Tanh_06", algorithm="deeplearning")
public class h2o_nn_32x6_Tanh_06 extends GenModel {
  public hex.ModelCategory getModelCategory() { return hex.ModelCategory.Multinomial; }
  public boolean isSupervised() { return true; }
  public int nfeatures() { return 13; }
  public int nclasses() { return 6; }
  // Thread-local storage for input neuron activation values.
  final double[] NUMS = new double[13];
  static class NORMMUL implements java.io.Serializable {
    public static final double[] VALUES = null;
}
  static class NORMSUB implements java.io.Serializable {
    public static final double[] VALUES = null;
}
  // Workspace for categorical offsets.
  public static final int[] CATOFFSETS = {0};
  // Number of neurons for each layer.
  public static final int[] NEURONS = {13,32,6};
    // Thread-local storage for neuron activation values.
    final double[][] ACTIVATION = new double[][] {
      /* Input */ h2o_nn_32x6_Tanh_06_Activation_0.VALUES,
      /* Tanh */ h2o_nn_32x6_Tanh_06_Activation_1.VALUES,
      /* Softmax */ h2o_nn_32x6_Tanh_06_Activation_2.VALUES
    };
    // Neuron bias values.
    public static final double[][] BIAS = new double[][] {
      /* Input */ h2o_nn_32x6_Tanh_06_Bias_0.VALUES,
      /* Tanh */ h2o_nn_32x6_Tanh_06_Bias_1.VALUES,
      /* Softmax */ h2o_nn_32x6_Tanh_06_Bias_2.VALUES
    };
    // Connecting weights between neurons.
    public static final float[][] WEIGHT = new float[][] {
      /* Input */ h2o_nn_32x6_Tanh_06_Weight_0.VALUES,
      /* Tanh */ h2o_nn_32x6_Tanh_06_Weight_1.VALUES,
      /* Softmax */ h2o_nn_32x6_Tanh_06_Weight_2.VALUES
    };

  // Names of columns used by model.
  public static final String[] NAMES = NamesHolder_h2o_nn_32x6_Tanh_06.VALUES;
  // Number of output classes included in training data response column.
  public static final int NCLASSES = 6;

  // Column domains. The last array contains domain of response column.
  public static final String[][] DOMAINS = new String[][] {
    /* X5 */ null,
    /* X12 */ null,
    /* X15 */ null,
    /* X17 */ null,
    /* X18 */ null,
    /* X19 */ null,
    /* X20 */ null,
    /* X21 */ null,
    /* X23 */ null,
    /* X27 */ null,
    /* X28 */ null,
    /* X34 */ null,
    /* X35 */ null,
    /* Label */ h2o_nn_32x6_Tanh_06_ColInfo_13.VALUES
  };
  // Prior class distribution
  public static final double[] PRIOR_CLASS_DISTRIB = {0.25961538461538464,0.11538461538461539,0.11538461538461539,0.125,0.25961538461538464,0.125};
  // Class distribution used for model building
  public static final double[] MODEL_CLASS_DISTRIB = null;

  public h2o_nn_32x6_Tanh_06() { super(NAMES,DOMAINS); }
  public String getUUID() { return Long.toString(7215883907649094408L); }

  // Pass in data in a double[], pre-aligned to the Model's requirements.
  // Jam predictions into the preds[] array; preds[0] is reserved for the
  // main prediction (class for classifiers or value for regression),
  // and remaining columns hold a probability distribution for classifiers.
  public final double[] score0( double[] data, double[] preds ) {
    java.util.Arrays.fill(preds,0);
    java.util.Arrays.fill(NUMS,0);
    int i = 0, ncats = 0;
    final int n = data.length;
    for(; i<n; ++i) {
      NUMS[i] = Double.isNaN(data[i]) ? 0 : data[i];
    }
    java.util.Arrays.fill(ACTIVATION[0],0);
    for (i=0; i<NUMS.length; ++i) {
      ACTIVATION[0][CATOFFSETS[CATOFFSETS.length-1] + i] = Double.isNaN(NUMS[i]) ? 0 : NUMS[i];
    }
    for (i=1; i<ACTIVATION.length; ++i) {
      java.util.Arrays.fill(ACTIVATION[i],0);
      int cols = ACTIVATION[i-1].length;
      int rows = ACTIVATION[i].length;
      int extra=cols-cols%8;
      int multiple = (cols/8)*8-1;
      int idx = 0;
      float[] a = WEIGHT[i];
      double[] x = ACTIVATION[i-1];
      double[] y = BIAS[i];
      double[] res = ACTIVATION[i];
      for (int row=0; row<rows; ++row) {
        double psum0 = 0, psum1 = 0, psum2 = 0, psum3 = 0, psum4 = 0, psum5 = 0, psum6 = 0, psum7 = 0;
        for (int col = 0; col < multiple; col += 8) {
          int off = idx + col;
          psum0 += a[off    ] * x[col    ];
          psum1 += a[off + 1] * x[col + 1];
          psum2 += a[off + 2] * x[col + 2];
          psum3 += a[off + 3] * x[col + 3];
          psum4 += a[off + 4] * x[col + 4];
          psum5 += a[off + 5] * x[col + 5];
          psum6 += a[off + 6] * x[col + 6];
          psum7 += a[off + 7] * x[col + 7];
        }
        res[row] += psum0 + psum1 + psum2 + psum3;
        res[row] += psum4 + psum5 + psum6 + psum7;
        for (int col = extra; col < cols; col++)
          res[row] += a[idx + col] * x[col];
        res[row] += y[row];
        idx += cols;
      }
      if (i<ACTIVATION.length-1) {
        for (int r=0; r<ACTIVATION[i].length; ++r) {
          ACTIVATION[i][r] = 1 - 2 / (1 + Math.exp(2*ACTIVATION[i][r]));
        }
      }
      if (i == ACTIVATION.length-1) {
        double max = ACTIVATION[i][0];
        for (int r=1; r<ACTIVATION[i].length; r++) {
          if (ACTIVATION[i][r]>max) max = ACTIVATION[i][r];
        }
        double scale = 0;
        for (int r=0; r<ACTIVATION[i].length; r++) {
          ACTIVATION[i][r] = Math.exp(ACTIVATION[i][r] - max);
          scale += ACTIVATION[i][r];
        }
        for (int r=0; r<ACTIVATION[i].length; r++) {
          if (Double.isNaN(ACTIVATION[i][r]))
            throw new RuntimeException("Numerical instability, predicted NaN.");
          ACTIVATION[i][r] /= scale;
          preds[r+1] = ACTIVATION[i][r];
        }
      }
    }
    preds[0] = hex.genmodel.GenModel.getPrediction(preds, PRIOR_CLASS_DISTRIB, data, 0.5);
    return preds;
  }
}
// Neuron activation values for Input layer
class h2o_nn_32x6_Tanh_06_Activation_0 implements java.io.Serializable {
  public static final double[] VALUES = new double[13];
  static {
    h2o_nn_32x6_Tanh_06_Activation_0_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_Tanh_06_Activation_0_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.0;
      sa[1] = 0.0;
      sa[2] = 0.0;
      sa[3] = 0.0;
      sa[4] = 0.0;
      sa[5] = 0.0;
      sa[6] = 0.0;
      sa[7] = 0.0;
      sa[8] = 0.0;
      sa[9] = 0.0;
      sa[10] = 0.0;
      sa[11] = 0.0;
      sa[12] = 0.0;
    }
  }
}
// Neuron activation values for Tanh layer
class h2o_nn_32x6_Tanh_06_Activation_1 implements java.io.Serializable {
  public static final double[] VALUES = new double[32];
  static {
    h2o_nn_32x6_Tanh_06_Activation_1_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_Tanh_06_Activation_1_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.0;
      sa[1] = 0.0;
      sa[2] = 0.0;
      sa[3] = 0.0;
      sa[4] = 0.0;
      sa[5] = 0.0;
      sa[6] = 0.0;
      sa[7] = 0.0;
      sa[8] = 0.0;
      sa[9] = 0.0;
      sa[10] = 0.0;
      sa[11] = 0.0;
      sa[12] = 0.0;
      sa[13] = 0.0;
      sa[14] = 0.0;
      sa[15] = 0.0;
      sa[16] = 0.0;
      sa[17] = 0.0;
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
    }
  }
}
// Neuron activation values for Softmax layer
class h2o_nn_32x6_Tanh_06_Activation_2 implements java.io.Serializable {
  public static final double[] VALUES = new double[6];
  static {
    h2o_nn_32x6_Tanh_06_Activation_2_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_Tanh_06_Activation_2_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.0;
      sa[1] = 0.0;
      sa[2] = 0.0;
      sa[3] = 0.0;
      sa[4] = 0.0;
      sa[5] = 0.0;
    }
  }
}
// Neuron bias values for Input layer
class h2o_nn_32x6_Tanh_06_Bias_0 implements java.io.Serializable {
  public static final double[] VALUES = null;
}
// Neuron bias values for Tanh layer
class h2o_nn_32x6_Tanh_06_Bias_1 implements java.io.Serializable {
  public static final double[] VALUES = new double[32];
  static {
    h2o_nn_32x6_Tanh_06_Bias_1_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_Tanh_06_Bias_1_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.08843888343059995;
      sa[1] = 0.1174448151530541;
      sa[2] = -0.14117277291889216;
      sa[3] = -0.3076957406309979;
      sa[4] = 0.04748390460349059;
      sa[5] = 0.11125681326905326;
      sa[6] = 0.17129610312556964;
      sa[7] = -0.1527508853681166;
      sa[8] = -0.019312270718397757;
      sa[9] = 0.19527980499641429;
      sa[10] = 0.021475984709396566;
      sa[11] = 0.14740536697166937;
      sa[12] = -0.0671117073925084;
      sa[13] = 0.08674254460897743;
      sa[14] = -0.10444474632327425;
      sa[15] = 0.08801550372006883;
      sa[16] = 0.19199288218752497;
      sa[17] = -0.0565143671543358;
      sa[18] = -0.11553137516608665;
      sa[19] = -0.16343632986137108;
      sa[20] = 0.25133548520702115;
      sa[21] = -0.029627047440567298;
      sa[22] = -0.004895335870898809;
      sa[23] = 0.0710725022691174;
      sa[24] = -0.0676333482787464;
      sa[25] = 0.13883306875417356;
      sa[26] = 0.00653192774756178;
      sa[27] = 0.09420454983799072;
      sa[28] = -0.28631904697705524;
      sa[29] = -0.006022275030049221;
      sa[30] = 0.10556417156480669;
      sa[31] = 0.12019537478116893;
    }
  }
}
// Neuron bias values for Softmax layer
class h2o_nn_32x6_Tanh_06_Bias_2 implements java.io.Serializable {
  public static final double[] VALUES = new double[6];
  static {
    h2o_nn_32x6_Tanh_06_Bias_2_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_Tanh_06_Bias_2_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.2060199565081869;
      sa[1] = -0.36161829345724467;
      sa[2] = -0.13543473867649156;
      sa[3] = -0.04156548386964958;
      sa[4] = -0.12195411630351263;
      sa[5] = -0.20364792855515948;
    }
  }
}
class h2o_nn_32x6_Tanh_06_Weight_0 implements java.io.Serializable {
  public static final float[] VALUES = null;
}
// Neuron weights connecting Input and Tanh layer
class h2o_nn_32x6_Tanh_06_Weight_1 implements java.io.Serializable {
  public static final float[] VALUES = new float[416];
  static {
    h2o_nn_32x6_Tanh_06_Weight_1_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_Tanh_06_Weight_1_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = -0.0792245f;
      sa[1] = 0.30880862f;
      sa[2] = 0.033159345f;
      sa[3] = 0.0427042f;
      sa[4] = -0.3651259f;
      sa[5] = -0.26974607f;
      sa[6] = -0.02205267f;
      sa[7] = 0.2950633f;
      sa[8] = 0.073267095f;
      sa[9] = 0.0019116561f;
      sa[10] = 0.10699172f;
      sa[11] = 0.04644585f;
      sa[12] = -0.17449981f;
      sa[13] = -0.45360082f;
      sa[14] = -0.0685917f;
      sa[15] = -0.07691217f;
      sa[16] = -0.0047666607f;
      sa[17] = -0.13062093f;
      sa[18] = -0.22215591f;
      sa[19] = 0.02394801f;
      sa[20] = 0.084819846f;
      sa[21] = -0.39909723f;
      sa[22] = 0.27177185f;
      sa[23] = -0.21008751f;
      sa[24] = 0.10995851f;
      sa[25] = 0.422697f;
      sa[26] = -0.33319354f;
      sa[27] = 0.32556793f;
      sa[28] = 0.2571971f;
      sa[29] = -0.19796024f;
      sa[30] = -0.11105237f;
      sa[31] = 0.015193177f;
      sa[32] = 0.35100392f;
      sa[33] = -0.21212694f;
      sa[34] = 0.15330027f;
      sa[35] = -0.08159199f;
      sa[36] = 0.37577024f;
      sa[37] = 0.46347576f;
      sa[38] = -0.25654194f;
      sa[39] = 0.30898243f;
      sa[40] = 0.57827204f;
      sa[41] = 0.30840093f;
      sa[42] = -0.11373867f;
      sa[43] = -0.023719838f;
      sa[44] = 0.29896417f;
      sa[45] = -0.69999886f;
      sa[46] = 0.12229388f;
      sa[47] = 0.26362878f;
      sa[48] = 0.007091931f;
      sa[49] = -0.18142794f;
      sa[50] = 0.24971698f;
      sa[51] = -0.25878754f;
      sa[52] = -0.21492572f;
      sa[53] = 0.305278f;
      sa[54] = 0.06766648f;
      sa[55] = -0.21742782f;
      sa[56] = -0.6333336f;
      sa[57] = -0.07320001f;
      sa[58] = -0.18825692f;
      sa[59] = -0.17976937f;
      sa[60] = -0.3382137f;
      sa[61] = 0.40349984f;
      sa[62] = 0.35043672f;
      sa[63] = 0.14783514f;
      sa[64] = 0.042411968f;
      sa[65] = 0.15227933f;
      sa[66] = -0.11292653f;
      sa[67] = 0.12042584f;
      sa[68] = -0.15105064f;
      sa[69] = 0.2006342f;
      sa[70] = 0.03489f;
      sa[71] = 0.27849084f;
      sa[72] = -0.26094675f;
      sa[73] = 0.18439807f;
      sa[74] = -0.21723665f;
      sa[75] = 0.015142952f;
      sa[76] = -0.27648994f;
      sa[77] = 0.12545712f;
      sa[78] = -0.07678464f;
      sa[79] = -0.17213126f;
      sa[80] = -0.42781115f;
      sa[81] = -0.12657836f;
      sa[82] = 0.119051576f;
      sa[83] = 0.17049718f;
      sa[84] = 0.14407705f;
      sa[85] = -0.27815118f;
      sa[86] = 0.048058636f;
      sa[87] = 0.41994354f;
      sa[88] = 0.48822427f;
      sa[89] = -0.0056286966f;
      sa[90] = 0.015518496f;
      sa[91] = 0.27974913f;
      sa[92] = 0.33435032f;
      sa[93] = -0.026422514f;
      sa[94] = -0.10814039f;
      sa[95] = -0.04339672f;
      sa[96] = 0.09425474f;
      sa[97] = 0.1588592f;
      sa[98] = 0.30997565f;
      sa[99] = 0.0045251073f;
      sa[100] = -0.27670902f;
      sa[101] = -0.6875916f;
      sa[102] = -0.3412051f;
      sa[103] = -0.19076686f;
      sa[104] = 0.031252462f;
      sa[105] = -0.4457007f;
      sa[106] = -0.34553692f;
      sa[107] = 0.3306691f;
      sa[108] = 0.43492743f;
      sa[109] = -0.19923069f;
      sa[110] = -0.06910388f;
      sa[111] = 0.24769785f;
      sa[112] = 0.1230116f;
      sa[113] = 0.10307143f;
      sa[114] = 0.150334f;
      sa[115] = -0.1924909f;
      sa[116] = 0.18257266f;
      sa[117] = -0.11133926f;
      sa[118] = 0.054982644f;
      sa[119] = -0.23507194f;
      sa[120] = -0.04778092f;
      sa[121] = -0.22286342f;
      sa[122] = 0.31437334f;
      sa[123] = 0.15394154f;
      sa[124] = 0.27019116f;
      sa[125] = 0.35827968f;
      sa[126] = -0.30595714f;
      sa[127] = -0.33058608f;
      sa[128] = 0.24609731f;
      sa[129] = -0.07204082f;
      sa[130] = -0.24594948f;
      sa[131] = 0.595645f;
      sa[132] = 0.19621222f;
      sa[133] = -0.2244903f;
      sa[134] = -0.17703904f;
      sa[135] = -0.5202922f;
      sa[136] = -0.16994818f;
      sa[137] = 0.031568427f;
      sa[138] = 0.21387993f;
      sa[139] = -0.115076244f;
      sa[140] = 0.06350131f;
      sa[141] = -0.35712445f;
      sa[142] = 0.1821046f;
      sa[143] = -0.5203551f;
      sa[144] = 0.2278584f;
      sa[145] = -0.33669737f;
      sa[146] = -0.36086214f;
      sa[147] = -0.03890499f;
      sa[148] = 0.19536586f;
      sa[149] = 0.33302686f;
      sa[150] = 0.20365745f;
      sa[151] = -0.39633012f;
      sa[152] = 0.17803094f;
      sa[153] = 0.6057157f;
      sa[154] = 0.07096762f;
      sa[155] = -0.05145136f;
      sa[156] = 0.20796482f;
      sa[157] = -0.30357704f;
      sa[158] = 0.13189879f;
      sa[159] = 0.25550824f;
      sa[160] = 0.23136653f;
      sa[161] = -0.0017169236f;
      sa[162] = 0.1628266f;
      sa[163] = 0.07450497f;
      sa[164] = 0.31906402f;
      sa[165] = -0.010063878f;
      sa[166] = 0.1203794f;
      sa[167] = -0.077047355f;
      sa[168] = 0.08415369f;
      sa[169] = 0.08526704f;
      sa[170] = -0.16626824f;
      sa[171] = -0.2584688f;
      sa[172] = -0.07901967f;
      sa[173] = -0.4129642f;
      sa[174] = -0.1641045f;
      sa[175] = -0.017662888f;
      sa[176] = -0.05837193f;
      sa[177] = -0.26607597f;
      sa[178] = 0.11654695f;
      sa[179] = 0.6822417f;
      sa[180] = 0.094204605f;
      sa[181] = -0.41203472f;
      sa[182] = 0.02881095f;
      sa[183] = -0.058077265f;
      sa[184] = 0.4912388f;
      sa[185] = 0.38520926f;
      sa[186] = -0.05292868f;
      sa[187] = 0.16824847f;
      sa[188] = 0.014481838f;
      sa[189] = 0.37211204f;
      sa[190] = 0.15796433f;
      sa[191] = -0.1849098f;
      sa[192] = -0.3377892f;
      sa[193] = -0.10734068f;
      sa[194] = 0.3150858f;
      sa[195] = 0.43741584f;
      sa[196] = 0.35812414f;
      sa[197] = 0.025510415f;
      sa[198] = -0.005183633f;
      sa[199] = 0.10144682f;
      sa[200] = 0.04649215f;
      sa[201] = -0.43778408f;
      sa[202] = 0.28393176f;
      sa[203] = -0.0034819744f;
      sa[204] = -0.15379588f;
      sa[205] = 0.06998094f;
      sa[206] = 0.5719543f;
      sa[207] = -0.24773118f;
      sa[208] = -0.49786535f;
      sa[209] = -0.118138604f;
      sa[210] = -0.5945547f;
      sa[211] = -0.6134518f;
      sa[212] = 0.021373589f;
      sa[213] = -0.2067201f;
      sa[214] = -0.33627656f;
      sa[215] = -0.07146775f;
      sa[216] = -0.02900033f;
      sa[217] = 0.28531462f;
      sa[218] = -0.02383466f;
      sa[219] = 0.3868762f;
      sa[220] = -0.26868957f;
      sa[221] = -0.030660084f;
      sa[222] = 0.28918505f;
      sa[223] = 0.034251824f;
      sa[224] = 0.24056521f;
      sa[225] = 0.31809583f;
      sa[226] = -0.62711895f;
      sa[227] = 0.72598624f;
      sa[228] = 0.22057605f;
      sa[229] = 0.104373835f;
      sa[230] = 0.2903777f;
      sa[231] = -0.09624906f;
      sa[232] = -0.40384674f;
      sa[233] = -0.041083302f;
      sa[234] = 0.08762812f;
      sa[235] = 0.015212935f;
      sa[236] = 0.19074927f;
      sa[237] = -0.45233673f;
      sa[238] = -0.6885438f;
      sa[239] = 0.13247156f;
      sa[240] = -0.18992795f;
      sa[241] = -0.33527225f;
      sa[242] = 0.05587863f;
      sa[243] = -0.3598495f;
      sa[244] = -0.5461326f;
      sa[245] = -0.19979334f;
      sa[246] = -0.4790998f;
      sa[247] = -0.082419865f;
      sa[248] = 0.38472944f;
      sa[249] = 0.4732351f;
      sa[250] = -0.067834355f;
      sa[251] = -0.19049986f;
      sa[252] = -0.36525604f;
      sa[253] = -0.26032415f;
      sa[254] = -0.33759522f;
      sa[255] = 0.33062235f;
      sa[256] = -0.3472289f;
      sa[257] = 0.18171799f;
      sa[258] = -0.7388468f;
      sa[259] = 0.28521115f;
      sa[260] = -0.06917892f;
      sa[261] = -0.2999166f;
      sa[262] = -0.17117497f;
      sa[263] = 0.24894129f;
      sa[264] = 0.05947752f;
      sa[265] = 0.017422132f;
      sa[266] = -0.018946407f;
      sa[267] = -0.5126723f;
      sa[268] = 0.014660484f;
      sa[269] = -0.30190712f;
      sa[270] = -0.25101686f;
      sa[271] = 0.18105829f;
      sa[272] = -0.31045482f;
      sa[273] = -0.10136618f;
      sa[274] = 0.110353276f;
      sa[275] = 0.07501491f;
      sa[276] = 0.0944512f;
      sa[277] = -0.17791665f;
      sa[278] = 0.09140447f;
      sa[279] = 0.11993827f;
      sa[280] = -0.28278506f;
      sa[281] = 0.34572923f;
      sa[282] = -0.1866032f;
      sa[283] = -0.24911322f;
      sa[284] = -0.08831243f;
      sa[285] = 0.06851992f;
      sa[286] = 0.36429453f;
      sa[287] = 0.06459494f;
      sa[288] = -0.21607332f;
      sa[289] = -0.1448518f;
      sa[290] = -0.00813544f;
      sa[291] = -0.04163351f;
      sa[292] = 0.18924932f;
      sa[293] = 0.08571604f;
      sa[294] = 0.15079647f;
      sa[295] = 0.18495505f;
      sa[296] = 0.5017066f;
      sa[297] = 0.43736613f;
      sa[298] = -0.28402695f;
      sa[299] = 0.064791165f;
      sa[300] = 0.092061974f;
      sa[301] = 0.019111482f;
      sa[302] = 0.101467945f;
      sa[303] = 0.12789957f;
      sa[304] = -0.26685947f;
      sa[305] = -0.023459222f;
      sa[306] = 0.37770736f;
      sa[307] = -0.12997621f;
      sa[308] = 0.13847455f;
      sa[309] = -0.0479517f;
      sa[310] = 0.564399f;
      sa[311] = -0.09186637f;
      sa[312] = -0.2994532f;
      sa[313] = -0.4286793f;
      sa[314] = -0.24610147f;
      sa[315] = 0.09100234f;
      sa[316] = -0.052813552f;
      sa[317] = -0.052673794f;
      sa[318] = 0.0012670871f;
      sa[319] = 0.29484227f;
      sa[320] = -0.08652241f;
      sa[321] = 0.21663854f;
      sa[322] = 0.4962293f;
      sa[323] = -0.3349357f;
      sa[324] = 0.450779f;
      sa[325] = 0.22900511f;
      sa[326] = -0.67906016f;
      sa[327] = -0.32822436f;
      sa[328] = 0.08829677f;
      sa[329] = 0.45798135f;
      sa[330] = -0.41606757f;
      sa[331] = 1.0601809f;
      sa[332] = -0.34689063f;
      sa[333] = -0.060022913f;
      sa[334] = -1.7988494E-4f;
      sa[335] = 0.75069845f;
      sa[336] = -0.2948026f;
      sa[337] = -0.26428914f;
      sa[338] = 0.3254335f;
      sa[339] = -0.31914616f;
      sa[340] = -0.32872275f;
      sa[341] = -0.30904752f;
      sa[342] = 0.21434465f;
      sa[343] = 0.063755885f;
      sa[344] = -0.42023614f;
      sa[345] = 0.14416972f;
      sa[346] = 0.23875177f;
      sa[347] = -0.25931644f;
      sa[348] = 0.23905122f;
      sa[349] = -0.412232f;
      sa[350] = -0.03126087f;
      sa[351] = 0.09321899f;
      sa[352] = 0.52605724f;
      sa[353] = -0.10197626f;
      sa[354] = -0.0049205828f;
      sa[355] = 0.06693674f;
      sa[356] = -0.3274029f;
      sa[357] = 0.43195444f;
      sa[358] = -0.37911576f;
      sa[359] = 0.012336979f;
      sa[360] = 0.10077748f;
      sa[361] = -0.111965254f;
      sa[362] = -0.1534402f;
      sa[363] = -0.10323867f;
      sa[364] = 0.019107506f;
      sa[365] = -0.27734298f;
      sa[366] = 0.10681178f;
      sa[367] = -0.16978574f;
      sa[368] = 0.08196412f;
      sa[369] = 0.28182653f;
      sa[370] = -0.10512149f;
      sa[371] = 0.27357873f;
      sa[372] = 0.054614935f;
      sa[373] = 0.26771462f;
      sa[374] = -0.29039755f;
      sa[375] = -0.2475929f;
      sa[376] = -0.43133834f;
      sa[377] = -0.27519432f;
      sa[378] = 0.6872443f;
      sa[379] = 0.17814282f;
      sa[380] = -0.05709013f;
      sa[381] = -0.20192692f;
      sa[382] = -0.30124527f;
      sa[383] = 0.18408862f;
      sa[384] = -0.12579551f;
      sa[385] = 0.20323968f;
      sa[386] = -0.36937413f;
      sa[387] = -0.32244903f;
      sa[388] = -0.010783477f;
      sa[389] = -0.14040527f;
      sa[390] = -0.04073223f;
      sa[391] = -0.17456649f;
      sa[392] = 0.14288838f;
      sa[393] = 0.118932225f;
      sa[394] = -0.046035837f;
      sa[395] = 0.53080225f;
      sa[396] = 0.2392291f;
      sa[397] = -0.45094335f;
      sa[398] = -0.009609085f;
      sa[399] = 0.14933752f;
      sa[400] = 0.14469855f;
      sa[401] = 0.38795227f;
      sa[402] = -0.13808553f;
      sa[403] = 0.4008405f;
      sa[404] = 0.49368572f;
      sa[405] = 0.054560192f;
      sa[406] = 0.02262856f;
      sa[407] = 0.41638133f;
      sa[408] = -0.30094013f;
      sa[409] = -0.22303493f;
      sa[410] = 0.030264836f;
      sa[411] = 0.4110676f;
      sa[412] = -0.43348664f;
      sa[413] = -0.67500645f;
      sa[414] = 0.024923014f;
      sa[415] = 0.24454679f;
    }
  }
}
// Neuron weights connecting Tanh and Softmax layer
class h2o_nn_32x6_Tanh_06_Weight_2 implements java.io.Serializable {
  public static final float[] VALUES = new float[192];
  static {
    h2o_nn_32x6_Tanh_06_Weight_2_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_Tanh_06_Weight_2_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = 1.0598997f;
      sa[1] = -0.18426959f;
      sa[2] = -0.10811427f;
      sa[3] = -0.8290959f;
      sa[4] = 0.0946f;
      sa[5] = 0.13142268f;
      sa[6] = 1.4815748f;
      sa[7] = -0.7082322f;
      sa[8] = -0.96696234f;
      sa[9] = 0.5017613f;
      sa[10] = -0.29843545f;
      sa[11] = 1.087414f;
      sa[12] = -0.23477454f;
      sa[13] = 0.3404867f;
      sa[14] = -1.5046198f;
      sa[15] = 1.0443084f;
      sa[16] = 2.276282f;
      sa[17] = -0.5371884f;
      sa[18] = 0.10526597f;
      sa[19] = -0.658678f;
      sa[20] = 1.4210881f;
      sa[21] = -0.15412308f;
      sa[22] = -0.40776885f;
      sa[23] = 0.75098926f;
      sa[24] = -1.4641224f;
      sa[25] = 0.7210217f;
      sa[26] = -0.15711468f;
      sa[27] = 1.4615088f;
      sa[28] = -1.3273475f;
      sa[29] = -0.5343064f;
      sa[30] = 0.5091394f;
      sa[31] = -0.7692113f;
      sa[32] = -1.2863613f;
      sa[33] = -1.4809585f;
      sa[34] = -0.06526837f;
      sa[35] = 1.4498724f;
      sa[36] = -0.65985936f;
      sa[37] = 0.023633428f;
      sa[38] = -0.21070577f;
      sa[39] = 0.82221705f;
      sa[40] = -0.66470253f;
      sa[41] = -1.1259489f;
      sa[42] = -1.1282935f;
      sa[43] = -1.5605377f;
      sa[44] = 0.28505322f;
      sa[45] = -1.5594397f;
      sa[46] = 1.4198831f;
      sa[47] = 1.5044893f;
      sa[48] = -1.412721f;
      sa[49] = -0.8511353f;
      sa[50] = 0.31164116f;
      sa[51] = -0.451207f;
      sa[52] = 0.84839934f;
      sa[53] = 0.8261451f;
      sa[54] = -0.3766974f;
      sa[55] = 1.180449f;
      sa[56] = -1.3454912f;
      sa[57] = -1.0981383f;
      sa[58] = 0.10241488f;
      sa[59] = -0.22544713f;
      sa[60] = 1.3408687f;
      sa[61] = -0.54815227f;
      sa[62] = -0.31029308f;
      sa[63] = 0.8275286f;
      sa[64] = 1.6743504f;
      sa[65] = -0.7226286f;
      sa[66] = 0.97893244f;
      sa[67] = 1.858167f;
      sa[68] = 0.92518085f;
      sa[69] = -1.178676f;
      sa[70] = -1.5648237f;
      sa[71] = -0.31121394f;
      sa[72] = -1.3476444f;
      sa[73] = 1.0032773f;
      sa[74] = 1.509172f;
      sa[75] = 0.022300689f;
      sa[76] = 0.5010171f;
      sa[77] = 0.6652124f;
      sa[78] = -1.2687094f;
      sa[79] = 1.3052846f;
      sa[80] = -0.6380787f;
      sa[81] = 0.18587817f;
      sa[82] = 1.073721f;
      sa[83] = -0.4529419f;
      sa[84] = -1.5342847f;
      sa[85] = -1.0701656f;
      sa[86] = 0.43162245f;
      sa[87] = 1.2445893f;
      sa[88] = -1.716046f;
      sa[89] = -0.71429086f;
      sa[90] = -1.0927961f;
      sa[91] = 1.5727724f;
      sa[92] = -0.28976142f;
      sa[93] = 0.62724334f;
      sa[94] = -1.5726403f;
      sa[95] = -0.19308263f;
      sa[96] = -0.82384264f;
      sa[97] = 0.30571604f;
      sa[98] = -0.21001856f;
      sa[99] = -0.8675013f;
      sa[100] = -1.1320155f;
      sa[101] = 0.075679556f;
      sa[102] = 0.8469417f;
      sa[103] = 0.256212f;
      sa[104] = 0.160094f;
      sa[105] = -0.6833649f;
      sa[106] = 1.8226242f;
      sa[107] = 0.8819704f;
      sa[108] = 0.78490084f;
      sa[109] = -0.2700733f;
      sa[110] = 0.8336268f;
      sa[111] = -1.0264982f;
      sa[112] = -1.5796149f;
      sa[113] = 1.0220618f;
      sa[114] = -0.13825804f;
      sa[115] = 0.26100415f;
      sa[116] = 0.97594225f;
      sa[117] = 0.6000165f;
      sa[118] = -1.582614f;
      sa[119] = -0.89307874f;
      sa[120] = -0.037876066f;
      sa[121] = 1.6533514f;
      sa[122] = -0.06451842f;
      sa[123] = 1.3822526f;
      sa[124] = -0.9623911f;
      sa[125] = 0.85176295f;
      sa[126] = -1.6122825f;
      sa[127] = 0.6505973f;
      sa[128] = -0.84841913f;
      sa[129] = 0.7685189f;
      sa[130] = -0.48664367f;
      sa[131] = 0.85104984f;
      sa[132] = 0.687323f;
      sa[133] = 0.43077716f;
      sa[134] = 0.5120897f;
      sa[135] = -1.0541806f;
      sa[136] = 0.29499432f;
      sa[137] = -0.5447736f;
      sa[138] = 0.1585513f;
      sa[139] = 0.91009873f;
      sa[140] = -0.48779947f;
      sa[141] = -0.3360386f;
      sa[142] = 0.69239813f;
      sa[143] = -0.65997785f;
      sa[144] = 0.85934514f;
      sa[145] = -1.3453555f;
      sa[146] = 0.083883174f;
      sa[147] = -0.09760503f;
      sa[148] = -0.90754217f;
      sa[149] = 1.1771486f;
      sa[150] = -1.3861003f;
      sa[151] = -1.6007612f;
      sa[152] = 1.6639141f;
      sa[153] = -1.452404f;
      sa[154] = 1.2710065f;
      sa[155] = -1.7211845f;
      sa[156] = -1.42498f;
      sa[157] = -1.0985123f;
      sa[158] = -0.36797565f;
      sa[159] = -0.4523765f;
      sa[160] = 0.86078924f;
      sa[161] = -0.8533177f;
      sa[162] = 1.4714905f;
      sa[163] = -1.0301576f;
      sa[164] = 0.7190229f;
      sa[165] = -0.4961781f;
      sa[166] = 1.6039884f;
      sa[167] = -2.1382074f;
      sa[168] = 1.5926193f;
      sa[169] = -1.5778127f;
      sa[170] = 0.70725876f;
      sa[171] = 1.2618397f;
      sa[172] = 1.6481295f;
      sa[173] = 1.6141138f;
      sa[174] = -0.35367006f;
      sa[175] = -0.067904994f;
      sa[176] = -0.80374736f;
      sa[177] = 0.33220592f;
      sa[178] = -1.8829423f;
      sa[179] = -0.5758742f;
      sa[180] = -1.4408784f;
      sa[181] = -1.1459492f;
      sa[182] = 0.74844825f;
      sa[183] = -0.5371992f;
      sa[184] = 1.7512449f;
      sa[185] = 1.744056f;
      sa[186] = -0.5331074f;
      sa[187] = -1.0902038f;
      sa[188] = -0.5855419f;
      sa[189] = -0.962675f;
      sa[190] = -1.1138122f;
      sa[191] = -1.1173657f;
    }
  }
}
// The class representing training column names
class NamesHolder_h2o_nn_32x6_Tanh_06 implements java.io.Serializable {
  public static final String[] VALUES = new String[13];
  static {
    NamesHolder_h2o_nn_32x6_Tanh_06_0.fill(VALUES);
  }
  static final class NamesHolder_h2o_nn_32x6_Tanh_06_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "X5";
      sa[1] = "X12";
      sa[2] = "X15";
      sa[3] = "X17";
      sa[4] = "X18";
      sa[5] = "X19";
      sa[6] = "X20";
      sa[7] = "X21";
      sa[8] = "X23";
      sa[9] = "X27";
      sa[10] = "X28";
      sa[11] = "X34";
      sa[12] = "X35";
    }
  }
}
// The class representing column Label
class h2o_nn_32x6_Tanh_06_ColInfo_13 implements java.io.Serializable {
  public static final String[] VALUES = new String[6];
  static {
    h2o_nn_32x6_Tanh_06_ColInfo_13_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_Tanh_06_ColInfo_13_0 implements java.io.Serializable {
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


