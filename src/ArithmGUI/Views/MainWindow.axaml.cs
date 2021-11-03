using System;
using System.IO;
using System.Threading.Tasks;
using Avalonia.Controls;
using Avalonia.Interactivity;
using Avalonia.Markup.Xaml;
using Avalonia.Threading;
using LongArithm.Interpreter;


namespace ArithmGUI.Views
{
    public  class MainWindow : Window
    {
        private readonly TextBox _codeBox;
        private readonly TextBox _consoleBox;
        private readonly MenuItem _runButton;
        private string _openedFilePath;

        public MainWindow()
        {
            InitializeComponent();
            
            _codeBox = this.Find<TextBox>( "CodeBox");
            _consoleBox = this.Find<TextBox>("ConsoleBox");
            _runButton = this.FindControl<MenuItem>("RunButton");
            _openedFilePath = "";
        }

        private void InitializeComponent()
        {
            AvaloniaXamlLoader.Load(this);
        }

        public void Start(object sender, RoutedEventArgs e)
        {
            _consoleBox.Text = "Processing...";
            _runButton.IsEnabled = false;
            var task = new Task<string>(() =>
            {
                try
                {
                    var res = Runners.run(_codeBox.Text);
                    var buffer = res.OutputBuffer;
                    var resString = "";
                    while (buffer.Count > 0)
                    {
                        resString += buffer.Dequeue() + "\n";
                    }
                    return "Process finished!\n" + resString;
                }
                catch (Exception ex)
                {
                    return ex.Message;
                }
            });
            task.ContinueWith(x => 
                Dispatcher.UIThread.Post(() =>
                {
                    _consoleBox.Text = x.Result;
                    _runButton.IsEnabled = true;
                })
            );
            task.Start();
        }
        
        public async void Open(object sender, RoutedEventArgs e)
        {
            var dialog = new OpenFileDialog();
            dialog.Filters.Add(new FileDialogFilter { Extensions = { "txt" } });
            var path = await dialog.ShowAsync(this);
            if (path is not {Length: > 0}) return;
            _codeBox.Text = await File.ReadAllTextAsync(path[0]);
            _openedFilePath = path[0];
        }

        private async void Save(object sender, RoutedEventArgs e)
        {
            var dialog = new SaveFileDialog
            {
                InitialFileName = _openedFilePath
            };
            var path = await dialog.ShowAsync(this);
            if (path == null) return;
            await File.WriteAllTextAsync(path, _codeBox.Text);
            _openedFilePath = path;
        }
        
        public void New(object sender, RoutedEventArgs e)
        {
            _codeBox.Text = "";
            _openedFilePath = "";
            Save(sender, e);
        }

    }
}
