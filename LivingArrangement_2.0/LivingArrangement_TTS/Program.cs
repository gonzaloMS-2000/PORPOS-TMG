using System;
using LivingArrangement_TTSML.Model;
using System.IO;

namespace LivingArrangement_TTS
{
    class Program
    {
        static void Main(string[] args)
        {

            string line;
            using (StreamReader reader = new StreamReader("../../../TTS_2016_ML_Format.csv"))
            {
                using (StreamWriter writer = new StreamWriter("../../../TTS_2016_LivingArrangements_2.0.csv"))
                {
                    reader.ReadLine();
                    while ((line = reader.ReadLine()) != null)
                    {
                        var parts = line.Split(',');

                        ModelInput input = new ModelInput
                        {
                            Home_Zone = float.Parse(parts[0]),
                            Cars = float.Parse(parts[6]),
                            Children = float.Parse(parts[9]),
                            Adults = float.Parse(parts[8]),
                            Income = float.Parse(parts[7]),
                            Gender = float.Parse(parts[2]),
                            Age = float.Parse(parts[1]),
                            Status = float.Parse(parts[5]),
                            License = float.Parse(parts[3]),
                            Distance = float.Parse(parts[11]),
                            Employment = float.Parse(parts[4]),
                            Mode = float.Parse(parts[10]),
                        };
                        var predictionResult = ConsumeModel.Predict(input);
                        writer.WriteLine(predictionResult.Prediction);
                    }
                }
            }
        }
    }
}
