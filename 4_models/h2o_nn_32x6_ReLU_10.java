/*
  Licensed under the Apache License, Version 2.0
    http://www.apache.org/licenses/LICENSE-2.0.html

  AUTOGENERATED BY H2O at 2016-12-26T14:43:05.790+01:00
  3.10.0.8
  
  Standalone prediction code with sample test data for DeepLearningModel named h2o_nn_32x6_ReLU_10

  How to download, compile and execute:
      mkdir tmpdir
      cd tmpdir
      curl http://127.0.0.1:54321/3/h2o-genmodel.jar > h2o-genmodel.jar
      curl http://127.0.0.1:54321/3/Models.java/h2o_nn_32x6_ReLU_10 > h2o_nn_32x6_ReLU_10.java
      javac -cp h2o-genmodel.jar -J-Xmx2g -J-XX:MaxPermSize=128m h2o_nn_32x6_ReLU_10.java

     (Note:  Try java argument -XX:+PrintCompilation to show runtime JIT compiler behavior.)
*/
import java.util.Map;
import hex.genmodel.GenModel;
import hex.genmodel.annotations.ModelPojo;

@ModelPojo(name="h2o_nn_32x6_ReLU_10", algorithm="deeplearning")
public class h2o_nn_32x6_ReLU_10 extends GenModel {
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
      /* Input */ h2o_nn_32x6_ReLU_10_Activation_0.VALUES,
      /* Rectifier */ h2o_nn_32x6_ReLU_10_Activation_1.VALUES,
      /* Softmax */ h2o_nn_32x6_ReLU_10_Activation_2.VALUES
    };
    // Neuron bias values.
    public static final double[][] BIAS = new double[][] {
      /* Input */ h2o_nn_32x6_ReLU_10_Bias_0.VALUES,
      /* Rectifier */ h2o_nn_32x6_ReLU_10_Bias_1.VALUES,
      /* Softmax */ h2o_nn_32x6_ReLU_10_Bias_2.VALUES
    };
    // Connecting weights between neurons.
    public static final float[][] WEIGHT = new float[][] {
      /* Input */ h2o_nn_32x6_ReLU_10_Weight_0.VALUES,
      /* Rectifier */ h2o_nn_32x6_ReLU_10_Weight_1.VALUES,
      /* Softmax */ h2o_nn_32x6_ReLU_10_Weight_2.VALUES
    };

  // Names of columns used by model.
  public static final String[] NAMES = NamesHolder_h2o_nn_32x6_ReLU_10.VALUES;
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
    /* Label */ h2o_nn_32x6_ReLU_10_ColInfo_13.VALUES
  };
  // Prior class distribution
  public static final double[] PRIOR_CLASS_DISTRIB = {0.24761904761904763,0.18571428571428572,0.06190476190476191,0.12857142857142856,0.24761904761904763,0.12857142857142856};
  // Class distribution used for model building
  public static final double[] MODEL_CLASS_DISTRIB = null;

  public h2o_nn_32x6_ReLU_10() { super(NAMES,DOMAINS); }
  public String getUUID() { return Long.toString(4135371232438993116L); }

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
          ACTIVATION[i][r] = Math.max(0, ACTIVATION[i][r]);
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
class h2o_nn_32x6_ReLU_10_Activation_0 implements java.io.Serializable {
  public static final double[] VALUES = new double[13];
  static {
    h2o_nn_32x6_ReLU_10_Activation_0_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_10_Activation_0_0 implements java.io.Serializable {
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
// Neuron activation values for Rectifier layer
class h2o_nn_32x6_ReLU_10_Activation_1 implements java.io.Serializable {
  public static final double[] VALUES = new double[32];
  static {
    h2o_nn_32x6_ReLU_10_Activation_1_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_10_Activation_1_0 implements java.io.Serializable {
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
class h2o_nn_32x6_ReLU_10_Activation_2 implements java.io.Serializable {
  public static final double[] VALUES = new double[6];
  static {
    h2o_nn_32x6_ReLU_10_Activation_2_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_10_Activation_2_0 implements java.io.Serializable {
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
class h2o_nn_32x6_ReLU_10_Bias_0 implements java.io.Serializable {
  public static final double[] VALUES = null;
}
// Neuron bias values for Rectifier layer
class h2o_nn_32x6_ReLU_10_Bias_1 implements java.io.Serializable {
  public static final double[] VALUES = new double[32];
  static {
    h2o_nn_32x6_ReLU_10_Bias_1_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_10_Bias_1_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.44737293743039086;
      sa[1] = 0.6048256104167923;
      sa[2] = 0.3762978892374196;
      sa[3] = 0.3603511617144182;
      sa[4] = 0.5276641844085326;
      sa[5] = 0.5387449901274192;
      sa[6] = 0.5625550053729975;
      sa[7] = 0.4118950560397401;
      sa[8] = 0.5935885673383415;
      sa[9] = 0.4792201007579876;
      sa[10] = 0.4138482184320014;
      sa[11] = 0.5949024505551671;
      sa[12] = 0.48080975273783416;
      sa[13] = 0.47484421389207404;
      sa[14] = 0.5584368510093862;
      sa[15] = 0.5336611289995539;
      sa[16] = 0.48788716610604466;
      sa[17] = 0.42347266863633415;
      sa[18] = 0.5243029061764892;
      sa[19] = 0.6689603947677469;
      sa[20] = 0.6362325305252385;
      sa[21] = 0.5584047670852372;
      sa[22] = 0.569899843578304;
      sa[23] = 0.5002549669640495;
      sa[24] = 0.6264809455719988;
      sa[25] = 0.5160026915555019;
      sa[26] = 0.5184229295180529;
      sa[27] = 0.4627373765360504;
      sa[28] = 0.5112074423776616;
      sa[29] = 0.5674879259946706;
      sa[30] = 0.5268078097609887;
      sa[31] = 0.5992757646587251;
    }
  }
}
// Neuron bias values for Softmax layer
class h2o_nn_32x6_ReLU_10_Bias_2 implements java.io.Serializable {
  public static final double[] VALUES = new double[6];
  static {
    h2o_nn_32x6_ReLU_10_Bias_2_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_10_Bias_2_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = -0.021197292561075144;
      sa[1] = -0.016183878233402907;
      sa[2] = -0.08704348803394073;
      sa[3] = -0.01997928740618182;
      sa[4] = -0.010391321670793525;
      sa[5] = -0.043468319089738534;
    }
  }
}
class h2o_nn_32x6_ReLU_10_Weight_0 implements java.io.Serializable {
  public static final float[] VALUES = null;
}
// Neuron weights connecting Input and Rectifier layer
class h2o_nn_32x6_ReLU_10_Weight_1 implements java.io.Serializable {
  public static final float[] VALUES = new float[416];
  static {
    h2o_nn_32x6_ReLU_10_Weight_1_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_10_Weight_1_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = 0.046918392f;
      sa[1] = 0.33260936f;
      sa[2] = 0.11173922f;
      sa[3] = 0.088351056f;
      sa[4] = -0.3558126f;
      sa[5] = -0.048626795f;
      sa[6] = -0.038292624f;
      sa[7] = 0.14592163f;
      sa[8] = 0.09290977f;
      sa[9] = -0.096454404f;
      sa[10] = -0.14150009f;
      sa[11] = -0.016714953f;
      sa[12] = -0.07452172f;
      sa[13] = -0.2852482f;
      sa[14] = -0.1767279f;
      sa[15] = 0.10436274f;
      sa[16] = -0.0028226688f;
      sa[17] = -0.022932226f;
      sa[18] = -0.19628894f;
      sa[19] = 0.4597274f;
      sa[20] = 0.35225555f;
      sa[21] = -0.25999677f;
      sa[22] = 0.282423f;
      sa[23] = -0.42393577f;
      sa[24] = 0.04360983f;
      sa[25] = 0.51241726f;
      sa[26] = -0.37226093f;
      sa[27] = 0.34604466f;
      sa[28] = 0.05711831f;
      sa[29] = -0.26357588f;
      sa[30] = -0.094825074f;
      sa[31] = 0.1668291f;
      sa[32] = 0.3017671f;
      sa[33] = -0.38300553f;
      sa[34] = 0.0017198621f;
      sa[35] = -0.20253672f;
      sa[36] = 0.22065216f;
      sa[37] = 0.48749125f;
      sa[38] = -0.15344828f;
      sa[39] = 0.17092687f;
      sa[40] = 0.11094889f;
      sa[41] = -0.23837514f;
      sa[42] = -0.13419546f;
      sa[43] = 0.018134227f;
      sa[44] = 0.32959527f;
      sa[45] = -0.52814597f;
      sa[46] = 0.036003623f;
      sa[47] = -0.08408688f;
      sa[48] = 0.258443f;
      sa[49] = 0.15440296f;
      sa[50] = 0.2560287f;
      sa[51] = -0.35390168f;
      sa[52] = -0.030569484f;
      sa[53] = 0.18287651f;
      sa[54] = 0.15702417f;
      sa[55] = -0.138224f;
      sa[56] = -0.48361468f;
      sa[57] = 0.0331394f;
      sa[58] = 0.10358093f;
      sa[59] = -0.28949878f;
      sa[60] = -0.35133687f;
      sa[61] = 0.25966668f;
      sa[62] = 0.19452459f;
      sa[63] = 0.093696f;
      sa[64] = 0.09921522f;
      sa[65] = 0.1644491f;
      sa[66] = -0.015975278f;
      sa[67] = 0.3378242f;
      sa[68] = -0.1667843f;
      sa[69] = 0.039183095f;
      sa[70] = -0.081905894f;
      sa[71] = 0.29392692f;
      sa[72] = -0.12777197f;
      sa[73] = 0.34786013f;
      sa[74] = -0.18583083f;
      sa[75] = -0.091251776f;
      sa[76] = -0.37077987f;
      sa[77] = 0.15442821f;
      sa[78] = -0.03575001f;
      sa[79] = -0.03887578f;
      sa[80] = 0.07991466f;
      sa[81] = -0.12797539f;
      sa[82] = 0.04107228f;
      sa[83] = 0.078458f;
      sa[84] = 0.18342523f;
      sa[85] = -0.17484336f;
      sa[86] = 0.36733145f;
      sa[87] = 0.3352289f;
      sa[88] = 0.48159915f;
      sa[89] = -0.17191039f;
      sa[90] = 0.043477733f;
      sa[91] = 0.11929035f;
      sa[92] = 0.2555826f;
      sa[93] = -0.18894163f;
      sa[94] = 0.037216388f;
      sa[95] = -0.003479775f;
      sa[96] = -0.029018687f;
      sa[97] = -0.09141336f;
      sa[98] = 0.28008452f;
      sa[99] = -0.17290165f;
      sa[100] = -0.041577976f;
      sa[101] = -0.29851323f;
      sa[102] = -0.29726818f;
      sa[103] = -0.23420978f;
      sa[104] = 0.08759124f;
      sa[105] = -0.35614032f;
      sa[106] = -0.33925647f;
      sa[107] = 0.21790493f;
      sa[108] = 0.2746833f;
      sa[109] = -0.3028348f;
      sa[110] = 0.006444936f;
      sa[111] = 0.27438664f;
      sa[112] = 0.21517344f;
      sa[113] = -0.107383125f;
      sa[114] = 0.07598743f;
      sa[115] = -0.23905739f;
      sa[116] = 0.068079054f;
      sa[117] = -0.10646734f;
      sa[118] = -0.083785616f;
      sa[119] = -0.1508738f;
      sa[120] = 0.037828714f;
      sa[121] = 0.020651974f;
      sa[122] = 0.531139f;
      sa[123] = 0.4421914f;
      sa[124] = 0.24007389f;
      sa[125] = 0.33040315f;
      sa[126] = -0.17920561f;
      sa[127] = -0.5333335f;
      sa[128] = 0.19164364f;
      sa[129] = -0.0060949046f;
      sa[130] = -0.24348049f;
      sa[131] = 0.2806717f;
      sa[132] = 0.09032757f;
      sa[133] = -0.27852243f;
      sa[134] = 0.31926802f;
      sa[135] = -0.38753644f;
      sa[136] = -0.019540368f;
      sa[137] = 0.029725468f;
      sa[138] = 0.0614151f;
      sa[139] = -0.19610418f;
      sa[140] = 0.12541315f;
      sa[141] = -0.38493362f;
      sa[142] = 0.32116458f;
      sa[143] = -0.3423493f;
      sa[144] = 0.27875954f;
      sa[145] = -0.11814734f;
      sa[146] = -0.4215809f;
      sa[147] = 0.07580141f;
      sa[148] = 0.23844011f;
      sa[149] = 0.4437481f;
      sa[150] = 0.41999403f;
      sa[151] = -0.2141294f;
      sa[152] = -3.7661503E-4f;
      sa[153] = 0.22422056f;
      sa[154] = 0.053486306f;
      sa[155] = -0.02787647f;
      sa[156] = 0.13992444f;
      sa[157] = -0.22307576f;
      sa[158] = 0.01149163f;
      sa[159] = 0.17769752f;
      sa[160] = 0.25790274f;
      sa[161] = 0.1981584f;
      sa[162] = -0.082550265f;
      sa[163] = -0.1163789f;
      sa[164] = 0.19567356f;
      sa[165] = -0.065689124f;
      sa[166] = 0.0823746f;
      sa[167] = -0.022396823f;
      sa[168] = -0.10403078f;
      sa[169] = 0.32479462f;
      sa[170] = -0.02201925f;
      sa[171] = -0.23166153f;
      sa[172] = -0.19548254f;
      sa[173] = -0.31071457f;
      sa[174] = 0.04233899f;
      sa[175] = -0.07261987f;
      sa[176] = -0.12245551f;
      sa[177] = -0.13671547f;
      sa[178] = -0.063665114f;
      sa[179] = 0.32945943f;
      sa[180] = 0.021765858f;
      sa[181] = -0.34691724f;
      sa[182] = -0.10655579f;
      sa[183] = -0.02539795f;
      sa[184] = 0.37024525f;
      sa[185] = 0.3450947f;
      sa[186] = -0.1522931f;
      sa[187] = -0.08514675f;
      sa[188] = -0.09553067f;
      sa[189] = 0.43075198f;
      sa[190] = 0.086841054f;
      sa[191] = -0.15572509f;
      sa[192] = 0.010447021f;
      sa[193] = 0.106232956f;
      sa[194] = 0.29336917f;
      sa[195] = 0.34282887f;
      sa[196] = 0.36870095f;
      sa[197] = -0.03399798f;
      sa[198] = 0.0961483f;
      sa[199] = -0.08744643f;
      sa[200] = 0.06596695f;
      sa[201] = -0.7504809f;
      sa[202] = 0.0118815135f;
      sa[203] = -0.1142286f;
      sa[204] = -0.047769006f;
      sa[205] = 0.26336268f;
      sa[206] = 0.5364926f;
      sa[207] = -0.16171539f;
      sa[208] = -0.32275593f;
      sa[209] = -0.046057053f;
      sa[210] = -0.24907063f;
      sa[211] = -0.49036917f;
      sa[212] = -0.094204985f;
      sa[213] = -0.13342334f;
      sa[214] = -0.20504221f;
      sa[215] = -0.010936514f;
      sa[216] = 0.16438152f;
      sa[217] = 0.31443614f;
      sa[218] = -0.47859278f;
      sa[219] = 0.30124167f;
      sa[220] = -0.29466742f;
      sa[221] = -0.035427593f;
      sa[222] = 0.21581858f;
      sa[223] = -0.012368698f;
      sa[224] = 0.18453592f;
      sa[225] = 0.46431524f;
      sa[226] = -0.32581452f;
      sa[227] = 0.6435416f;
      sa[228] = 0.14251985f;
      sa[229] = 0.030248951f;
      sa[230] = 0.25579265f;
      sa[231] = -0.1862403f;
      sa[232] = -0.41764885f;
      sa[233] = 0.10169227f;
      sa[234] = 0.12934598f;
      sa[235] = -0.15563548f;
      sa[236] = -0.011150984f;
      sa[237] = -0.46816862f;
      sa[238] = -0.46972853f;
      sa[239] = 0.31395373f;
      sa[240] = -0.22622879f;
      sa[241] = -0.19180933f;
      sa[242] = -0.09053418f;
      sa[243] = -0.17762889f;
      sa[244] = -0.48124662f;
      sa[245] = -0.09561433f;
      sa[246] = -0.7558629f;
      sa[247] = 0.013698583f;
      sa[248] = 0.18552491f;
      sa[249] = 0.48215717f;
      sa[250] = -0.108678944f;
      sa[251] = 0.09132911f;
      sa[252] = -0.4580656f;
      sa[253] = -0.37029782f;
      sa[254] = -0.096661255f;
      sa[255] = 0.3520459f;
      sa[256] = -0.30577645f;
      sa[257] = 0.61009717f;
      sa[258] = -0.45253393f;
      sa[259] = 0.07641451f;
      sa[260] = -0.27095535f;
      sa[261] = -0.050400976f;
      sa[262] = 0.12582928f;
      sa[263] = 0.3127348f;
      sa[264] = -0.08177799f;
      sa[265] = -0.07111214f;
      sa[266] = -0.40574405f;
      sa[267] = -0.2313875f;
      sa[268] = 0.21877605f;
      sa[269] = -0.026581563f;
      sa[270] = -0.1354385f;
      sa[271] = 0.114234075f;
      sa[272] = -0.1939561f;
      sa[273] = -0.11937513f;
      sa[274] = 0.09764284f;
      sa[275] = 0.14332731f;
      sa[276] = 0.12047076f;
      sa[277] = -0.2950205f;
      sa[278] = -0.15014519f;
      sa[279] = 0.12575987f;
      sa[280] = -0.13744622f;
      sa[281] = 0.41427135f;
      sa[282] = -0.10870203f;
      sa[283] = -0.17869899f;
      sa[284] = -0.15169358f;
      sa[285] = 0.18324754f;
      sa[286] = 0.33125955f;
      sa[287] = 0.057659287f;
      sa[288] = -0.39046016f;
      sa[289] = -0.060998965f;
      sa[290] = 0.0033546574f;
      sa[291] = 0.038536783f;
      sa[292] = -0.1903499f;
      sa[293] = -0.20284775f;
      sa[294] = -0.017123712f;
      sa[295] = 0.12685616f;
      sa[296] = 0.5544008f;
      sa[297] = 0.39906517f;
      sa[298] = -0.38257372f;
      sa[299] = -0.025869297f;
      sa[300] = 0.09637454f;
      sa[301] = -0.06736624f;
      sa[302] = 0.1402818f;
      sa[303] = 0.01421655f;
      sa[304] = -0.19227546f;
      sa[305] = -0.41888016f;
      sa[306] = 0.17017609f;
      sa[307] = -0.23929782f;
      sa[308] = 0.22890846f;
      sa[309] = 0.11724294f;
      sa[310] = 0.6364329f;
      sa[311] = -0.02933814f;
      sa[312] = -0.17482813f;
      sa[313] = -0.3629779f;
      sa[314] = -0.24831636f;
      sa[315] = -0.00219844f;
      sa[316] = -0.0664083f;
      sa[317] = -0.08906439f;
      sa[318] = 0.30369642f;
      sa[319] = 0.41450676f;
      sa[320] = 0.006711048f;
      sa[321] = 0.048891135f;
      sa[322] = 0.37275368f;
      sa[323] = -0.33054852f;
      sa[324] = 0.3356193f;
      sa[325] = 0.2966197f;
      sa[326] = -0.34907466f;
      sa[327] = 0.024059726f;
      sa[328] = -0.07978607f;
      sa[329] = 0.38377383f;
      sa[330] = -0.2650584f;
      sa[331] = 0.25832084f;
      sa[332] = -0.3123933f;
      sa[333] = 0.1195271f;
      sa[334] = -0.07315147f;
      sa[335] = 0.45336908f;
      sa[336] = -0.357816f;
      sa[337] = -0.17369719f;
      sa[338] = 0.35080996f;
      sa[339] = -0.30216366f;
      sa[340] = -0.29339963f;
      sa[341] = -0.28948757f;
      sa[342] = 0.13596657f;
      sa[343] = -0.12023289f;
      sa[344] = -0.1891634f;
      sa[345] = 0.2948124f;
      sa[346] = 0.30595708f;
      sa[347] = -0.26538146f;
      sa[348] = 0.24668255f;
      sa[349] = -0.52544373f;
      sa[350] = -0.02309781f;
      sa[351] = 0.07743474f;
      sa[352] = 0.4480167f;
      sa[353] = 0.018067619f;
      sa[354] = 0.041887812f;
      sa[355] = 0.10909784f;
      sa[356] = -0.1536621f;
      sa[357] = 0.27279794f;
      sa[358] = -0.37018144f;
      sa[359] = 0.0051821377f;
      sa[360] = 0.2178825f;
      sa[361] = -0.2597992f;
      sa[362] = -0.035679877f;
      sa[363] = -0.190257f;
      sa[364] = -0.077521846f;
      sa[365] = -0.27934644f;
      sa[366] = -0.04209189f;
      sa[367] = -0.06673905f;
      sa[368] = 0.042239953f;
      sa[369] = 0.18696432f;
      sa[370] = -0.51065856f;
      sa[371] = 0.13241561f;
      sa[372] = -0.09273054f;
      sa[373] = 0.33769184f;
      sa[374] = 0.15934834f;
      sa[375] = -0.24459583f;
      sa[376] = -0.42094514f;
      sa[377] = -0.3023086f;
      sa[378] = 0.5256174f;
      sa[379] = 0.08699807f;
      sa[380] = -0.07797053f;
      sa[381] = 0.06420783f;
      sa[382] = -0.13079265f;
      sa[383] = 0.36855486f;
      sa[384] = -0.13730389f;
      sa[385] = 0.07180043f;
      sa[386] = -0.330646f;
      sa[387] = -0.29412323f;
      sa[388] = 0.016579626f;
      sa[389] = -0.23234738f;
      sa[390] = 0.026095418f;
      sa[391] = -0.047818676f;
      sa[392] = 0.47913823f;
      sa[393] = 0.1466858f;
      sa[394] = -0.2739074f;
      sa[395] = 0.4164418f;
      sa[396] = 0.15112282f;
      sa[397] = -0.34464157f;
      sa[398] = 0.2539451f;
      sa[399] = 0.25048065f;
      sa[400] = 0.021411533f;
      sa[401] = 0.32018334f;
      sa[402] = -0.16631995f;
      sa[403] = 0.059528235f;
      sa[404] = 0.3529131f;
      sa[405] = -0.21069144f;
      sa[406] = 0.037691336f;
      sa[407] = 0.39602545f;
      sa[408] = -0.44360644f;
      sa[409] = -0.49195656f;
      sa[410] = -0.036661185f;
      sa[411] = 0.1929372f;
      sa[412] = -0.18724181f;
      sa[413] = 0.12511118f;
      sa[414] = 0.10394909f;
      sa[415] = 0.21613252f;
    }
  }
}
// Neuron weights connecting Rectifier and Softmax layer
class h2o_nn_32x6_ReLU_10_Weight_2 implements java.io.Serializable {
  public static final float[] VALUES = new float[192];
  static {
    h2o_nn_32x6_ReLU_10_Weight_2_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_10_Weight_2_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = 1.2963183f;
      sa[1] = -0.17164937f;
      sa[2] = -0.4840031f;
      sa[3] = -0.8691675f;
      sa[4] = 0.14188965f;
      sa[5] = 0.17648527f;
      sa[6] = 1.1559108f;
      sa[7] = -0.618988f;
      sa[8] = -1.0202042f;
      sa[9] = 0.62110984f;
      sa[10] = -0.27128816f;
      sa[11] = 0.4075825f;
      sa[12] = 0.16232751f;
      sa[13] = 0.10032648f;
      sa[14] = -1.2069823f;
      sa[15] = 1.1041819f;
      sa[16] = 1.6270697f;
      sa[17] = -0.5753893f;
      sa[18] = 0.11843348f;
      sa[19] = -0.5084252f;
      sa[20] = 1.1856507f;
      sa[21] = 0.043434165f;
      sa[22] = -1.0163262f;
      sa[23] = 0.72133744f;
      sa[24] = -1.4169949f;
      sa[25] = 0.3138901f;
      sa[26] = -0.31603557f;
      sa[27] = 1.4721661f;
      sa[28] = -1.3674963f;
      sa[29] = -0.37570077f;
      sa[30] = 0.46301594f;
      sa[31] = -0.7956074f;
      sa[32] = -1.1879395f;
      sa[33] = -1.590228f;
      sa[34] = -0.045275897f;
      sa[35] = 1.38842f;
      sa[36] = -0.42319188f;
      sa[37] = -0.26790214f;
      sa[38] = 0.14243768f;
      sa[39] = 0.72006667f;
      sa[40] = -0.68481517f;
      sa[41] = -1.4713252f;
      sa[42] = -1.0422113f;
      sa[43] = -1.4533477f;
      sa[44] = 0.047318567f;
      sa[45] = -1.298095f;
      sa[46] = 1.1848193f;
      sa[47] = 1.4287518f;
      sa[48] = -1.3347297f;
      sa[49] = -1.1206402f;
      sa[50] = 0.3090769f;
      sa[51] = -0.25313544f;
      sa[52] = 0.85327876f;
      sa[53] = 0.7491537f;
      sa[54] = -0.20798959f;
      sa[55] = 1.2577189f;
      sa[56] = -1.1374747f;
      sa[57] = -1.0221062f;
      sa[58] = 0.21658537f;
      sa[59] = -0.2778582f;
      sa[60] = 1.3678466f;
      sa[61] = -0.67109203f;
      sa[62] = -0.40611777f;
      sa[63] = 0.8169625f;
      sa[64] = 1.380507f;
      sa[65] = -0.75600815f;
      sa[66] = 0.67888373f;
      sa[67] = 1.5888232f;
      sa[68] = 0.6682815f;
      sa[69] = -1.1403767f;
      sa[70] = -1.5337532f;
      sa[71] = -0.43467262f;
      sa[72] = -1.382069f;
      sa[73] = 1.1398058f;
      sa[74] = 1.1992204f;
      sa[75] = -0.048832923f;
      sa[76] = 0.793677f;
      sa[77] = 0.515583f;
      sa[78] = -1.3466631f;
      sa[79] = 0.9493973f;
      sa[80] = -0.7260918f;
      sa[81] = 0.07955744f;
      sa[82] = 0.84238225f;
      sa[83] = -0.73996365f;
      sa[84] = -1.5875062f;
      sa[85] = -1.320979f;
      sa[86] = 0.27809304f;
      sa[87] = 1.1855342f;
      sa[88] = -1.9118118f;
      sa[89] = -0.6468496f;
      sa[90] = -1.1363572f;
      sa[91] = 1.4554982f;
      sa[92] = -0.33460537f;
      sa[93] = 0.2981958f;
      sa[94] = -1.5126225f;
      sa[95] = -0.3374517f;
      sa[96] = -1.0825961f;
      sa[97] = 0.25592864f;
      sa[98] = -0.14127938f;
      sa[99] = 0.10917107f;
      sa[100] = -1.2519486f;
      sa[101] = -0.28289962f;
      sa[102] = 1.2587556f;
      sa[103] = 0.03920188f;
      sa[104] = -0.06601275f;
      sa[105] = -0.5338877f;
      sa[106] = 1.52501f;
      sa[107] = 1.1574409f;
      sa[108] = 0.700134f;
      sa[109] = -0.009609084f;
      sa[110] = 0.6505957f;
      sa[111] = -1.0201973f;
      sa[112] = -1.2427211f;
      sa[113] = 0.34910193f;
      sa[114] = 0.032805074f;
      sa[115] = -0.25147933f;
      sa[116] = 0.9655115f;
      sa[117] = 0.089750454f;
      sa[118] = -1.4444914f;
      sa[119] = -0.90347046f;
      sa[120] = -0.25901005f;
      sa[121] = 1.1187779f;
      sa[122] = -0.071855985f;
      sa[123] = 0.46473828f;
      sa[124] = -0.79091084f;
      sa[125] = 0.86515707f;
      sa[126] = -1.0441257f;
      sa[127] = 0.50616664f;
      sa[128] = -0.86771655f;
      sa[129] = 0.8561515f;
      sa[130] = -0.4752448f;
      sa[131] = 0.84462166f;
      sa[132] = 0.44323525f;
      sa[133] = 0.6194121f;
      sa[134] = 0.6364786f;
      sa[135] = -1.0730568f;
      sa[136] = 0.5069184f;
      sa[137] = -0.51753783f;
      sa[138] = -0.08958063f;
      sa[139] = 1.100945f;
      sa[140] = -0.4467125f;
      sa[141] = -0.086980954f;
      sa[142] = 0.3116139f;
      sa[143] = -0.81983656f;
      sa[144] = 0.9315022f;
      sa[145] = -0.73851854f;
      sa[146] = 0.11370173f;
      sa[147] = -0.3384093f;
      sa[148] = -0.8823181f;
      sa[149] = 1.1315306f;
      sa[150] = -1.1734642f;
      sa[151] = -1.6083215f;
      sa[152] = 1.5873151f;
      sa[153] = -0.6179904f;
      sa[154] = 1.2876773f;
      sa[155] = -1.5776138f;
      sa[156] = -1.4247847f;
      sa[157] = -1.1660638f;
      sa[158] = -0.23751597f;
      sa[159] = -0.67646086f;
      sa[160] = 1.1777456f;
      sa[161] = -1.1736938f;
      sa[162] = 1.4915867f;
      sa[163] = -0.7385607f;
      sa[164] = 0.6874247f;
      sa[165] = -0.58410656f;
      sa[166] = 1.4374505f;
      sa[167] = -1.5538771f;
      sa[168] = 1.0251169f;
      sa[169] = -1.5978904f;
      sa[170] = 1.0587622f;
      sa[171] = 0.90663654f;
      sa[172] = 1.4993571f;
      sa[173] = 1.512578f;
      sa[174] = -0.22244328f;
      sa[175] = -0.007035557f;
      sa[176] = -0.98217726f;
      sa[177] = -0.0035033328f;
      sa[178] = -1.2183788f;
      sa[179] = -0.22735512f;
      sa[180] = -1.4589782f;
      sa[181] = -1.0482081f;
      sa[182] = 0.49480563f;
      sa[183] = -0.9186435f;
      sa[184] = 1.5073391f;
      sa[185] = 1.3127408f;
      sa[186] = -0.44218364f;
      sa[187] = -1.0988787f;
      sa[188] = -0.4996411f;
      sa[189] = -0.90106183f;
      sa[190] = -1.480241f;
      sa[191] = -0.9464446f;
    }
  }
}
// The class representing training column names
class NamesHolder_h2o_nn_32x6_ReLU_10 implements java.io.Serializable {
  public static final String[] VALUES = new String[13];
  static {
    NamesHolder_h2o_nn_32x6_ReLU_10_0.fill(VALUES);
  }
  static final class NamesHolder_h2o_nn_32x6_ReLU_10_0 implements java.io.Serializable {
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
class h2o_nn_32x6_ReLU_10_ColInfo_13 implements java.io.Serializable {
  public static final String[] VALUES = new String[6];
  static {
    h2o_nn_32x6_ReLU_10_ColInfo_13_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_10_ColInfo_13_0 implements java.io.Serializable {
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


