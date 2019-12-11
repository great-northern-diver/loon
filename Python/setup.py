import setuptools

with open("README.md", "r") as fh:
    long_description = fh.read()

setuptools.setup(
    name="diver", # Replace with your own username
    version="1.2.8",
    author="Example Author",
    author_email="author@example.com",
    description="A small example package",
    long_description= "test",
    url="https://github.com/pw2wang/loon",
    packages=setuptools.find_packages(),
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
    ],
    install_requires = [
        'numpy',
        'pandas',
        'sklearn',
        'matplotlib',
        'multipleddispatch'
    ],
    include_package_data=True,
    python_requires='>=3.6',
)